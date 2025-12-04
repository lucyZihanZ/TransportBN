casualty_var <- "casualty_count_cat"
casualty_levels <- levels(data_bn[[casualty_var]])
target_level <- casualty_levels[length(casualty_levels)]  # Highest casualty level

imd_var <- "imd_cat_3"
imd_levels <- levels(data_bn[[imd_var]])
imd_low <- imd_levels[1]
imd_high <- imd_levels[length(imd_levels)]

analyze_factor_boot <- function(factor_var,
                                factor_name = factor_var,
                                data_bn,
                                dag,
                                base_evidence = list(),
                                stratify_by = NULL,
                                B = 200,
                                cp_n = 5000,
                                cores = max(1, parallel::detectCores() - 1),
                                seed = 2025) {
  
  library(bnlearn); library(foreach); library(doParallel)
  library(dplyr); library(tidyr); library(tibble); library(rlang)
  
  set.seed(seed)
  
  # sanity checks
  if(!factor_var %in% names(data_bn)) stop(sprintf("Variable '%s' not found in data_bn", factor_var))
  if(!is.null(stratify_by) && ! stratify_by %in% names(data_bn)) stop(sprintf("Stratifier '%s' not found in data_bn", stratify_by))
  
  factor_levels <- levels(data_bn[[factor_var]])
  imd_levels_local <- imd_levels  # assumes imd_levels exists in your session
  
  # create combos
  if(is.null(stratify_by)) {
    combos <- expand.grid(imd = imd_levels_local, stringsAsFactors = FALSE)
    combos$group_label <- combos$imd
  } else {
    strat_levels <- levels(data_bn[[stratify_by]])
    combos <- expand.grid(imd = imd_levels_local, strat = strat_levels, stringsAsFactors = FALSE)
    combos$group_label <- paste0(combos$imd, "___", combos$strat)
  }
  
  K <- nrow(combos)
  
  # parallel setup
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  message(sprintf("Running %d bootstrap replicates × %d combos (total cpdist calls: ~ %d × cp_n)", B, K, B*K))
  
  # bootstrap iterations
  boot_mat <- foreach(bi = 1:B, .combine = rbind, .packages = "bnlearn") %dopar% {
    
    # resample rows with replacement
    idx <- sample(nrow(data_bn), nrow(data_bn), replace = TRUE)
    data_b <- data_bn[idx, , drop = FALSE]
    
    # refit parameters (bayes) with fixed DAG
    fitted_b <- bn.fit(dag, data_b, method = "bayes")
    
    # for each combo, sample factor_var with cpdist and compute proportions
    res_vec <- unlist(lapply(seq_len(K), function(j) {
      imd_j <- combos$imd[j]
      if(is.null(stratify_by)) {
        evidence_j <- c(base_evidence, list(imd_cat_3 = imd_j))
      } else {
        strat_j <- combos$strat[j]
        evidence_j <- c(base_evidence, list(imd_cat_3 = imd_j))
        evidence_j[[stratify_by]] <- strat_j
      }
      
      sims <- tryCatch(
        cpdist(fitted_b, nodes = factor_var, evidence = evidence_j, method = "lw", n = cp_n),
        error = function(e) return(NULL)
      )
      
      if(is.null(sims)) return(rep(NA_real_, length(factor_levels)))
      
      # proportion vector for all factor levels
      tab <- prop.table(table(factor(sims[[factor_var]], levels = factor_levels)))
      as.numeric(tab[factor_levels])
    }), use.names = FALSE)
    
    res_vec
  }
  
  stopCluster(cl)
  
  # summarize bootstrap results: mean + 95% CI
  L <- length(factor_levels)
  summary_rows <- lapply(seq_len(K), function(j) {
    cols_idx <- ((j-1)*L + 1):((j-1)*L + L)
    mat_j <- boot_mat[, cols_idx, drop = FALSE]
    res_mat <- t(apply(mat_j, 2, function(col) {
      col2 <- col[!is.na(col)]
      if(length(col2) == 0) return(c(mean=NA_real_, lower=NA_real_, upper=NA_real_))
      qs <- quantile(col2, probs = c(0.025,0.975), names = FALSE, type = 6)
      c(mean = mean(col2), lower = qs[1], upper = qs[2])
    }))
    colnames(res_mat) <- c("mean","lower","upper")
    dfj <- as.data.frame(res_mat, stringsAsFactors = FALSE)
    dfj$level <- factor_levels
    dfj
  })
  
  # build tidy data frame
  tidy_list <- lapply(seq_len(K), function(j) {
    dfj <- summary_rows[[j]]
    dfj$group_label <- combos$group_label[j]
    dfj
  })
  tidy_df <- bind_rows(tidy_list) %>%
    select(group_label, level, mean, lower, upper) %>%
    mutate(across(c(mean, lower, upper), as.numeric))
  
  # split group_label back into imd / strat
  if(is.null(stratify_by)) {
    tidy_df <- tidy_df %>%
      separate(group_label, into=c("imd"), sep="___", fill="right") %>%
      select(imd, level, mean, lower, upper)
  } else {
    tidy_df <- tidy_df %>%
      separate(group_label, into=c("imd","strat"), sep="___", fill="right") %>%
      select(imd, strat, level, mean, lower, upper)
  }
  
  # print pretty tables
  cat("\n\n", paste(rep("=",80),collapse=""), "\n")
  cat(sprintf("FACTOR ANALYSIS (BOOTSTRAP CIs): %s\n", factor_name))
  cat(paste(rep("=",80),collapse=""), "\n\n")
  cat("Factor levels:\n")
  print(factor_levels)
  cat("\n--- Exposure (proportions) with 95% bootstrap CIs ---\n\n")
  
  if(is.null(stratify_by)) {
    pretty <- tidy_df %>%
      mutate(label = sprintf("%.3f (%.3f, %.3f)", mean, lower, upper)) %>%
      pivot_wider(names_from = level, values_from = label) %>%
      arrange(match(imd, imd_levels_local))
    print(pretty)
  } else {
    for(strat_val in unique(tidy_df$strat)) {
      cat(sprintf("\n--- stratifier %s = %s ---\n", stratify_by, strat_val))
      pretty <- tidy_df %>%
        filter(strat == strat_val) %>%
        mutate(label = sprintf("%.3f (%.3f, %.3f)", mean, lower, upper)) %>%
        pivot_wider(names_from = level, values_from = label) %>%
        arrange(match(imd, imd_levels_local))
      print(pretty)
    }
  }
  
  invisible(tidy_df)
}


res <- analyze_factor_boot(
  factor_var = "light_conditions",
  factor_name = "Lighting Conditions",
  data_bn = data_bn,
  dag = dag,
  base_evidence = list(),  
  stratify_by = NULL,   
  B = 200,
  cp_n = 5000
)


# res has columns: imd, level (lighting level), mean, lower, upper
lighting_plot_df <- res %>%
  mutate(across(c(mean, lower, upper), as.numeric)) %>%
  rename(light_level = level)

ggplot(lighting_plot_df, aes(x = imd, y = mean, color = imd)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~light_level, scales = "free_y") +
  labs(x = "IMD Level", y = "Proportion of Accidents",
       title = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave("lighting_IMD.png", width = 8, height = 5, dpi = 300)

lighting_plot_df
