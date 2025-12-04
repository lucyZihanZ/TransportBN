library(bnlearn)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(tibble)

set.seed(2025)

vehicle_vars <- c("car_present", "motorcycle_present", "bicycle_present",
                  "bus_truck_present", "other_present")
imd_levels <- imd_levels  # assume this exists as in your session

# bootstrap + cpdist tuning (adjust for runtime)
B <- 200        # bootstrap replicates (reduce to 100 if slow)
cp_n <- 5000    # cpdist draws per query (reduce to 2000 if needed)
cores <- max(1, parallel::detectCores() - 1)

# create combos and helpers
combos <- expand.grid(imd = imd_levels, v = vehicle_vars, stringsAsFactors = FALSE)
K <- nrow(combos)

# get BN structure from fitted bn.fit
bn_structure <- dag

# parallel setup
cl <- makeCluster(cores)
registerDoParallel(cl)

# boot matrix: rows = bootstrap iterations, cols = combos
boot_mat <- foreach(b = 1:B, .combine = rbind, .packages = "bnlearn") %dopar% {
  # 1) resample rows with replacement
  idx <- sample(nrow(data_bn), nrow(data_bn), replace = TRUE)
  data_b <- data_bn[idx, , drop = FALSE]
  
  # 2) re-fit parameters (bayes) with the fixed structure
  fitted_b <- bn.fit(bn_structure, data_b, method = "bayes")
  
  # 3) for each combo, run cpdist and compute proportion of "Yes"
  sapply(seq_len(K), function(j) {
    imd_j <- combos$imd[j]
    v_j   <- combos$v[j]
    
    # run cpdist to sample that node given evidence
    sims <- tryCatch(
      cpdist(fitted_b,
             nodes = v_j,
             evidence = list(imd_cat_3 = imd_j),
             method = "lw",
             n = cp_n),
      error = function(e) return(NA)  # return NA if cpdist fails
    )
    
    if(is.na(sims)[1]) return(NA_real_)
    
    # determine "yes" level robustly (fall back to second level if none matched)
    v_levels <- levels(data_bn[[v_j]])
    yes_level <- v_levels[v_levels %in% c("Yes", "yes", "Y", "1", "TRUE", "True")]
    if(length(yes_level) == 0) {
      if(length(v_levels) >= 2) yes_level <- v_levels[2] else yes_level <- v_levels[1]
    }
    
    # compute proportion (safely handle possible zero counts)
    tab <- prop.table(table(factor(sims[[v_j]], levels = v_levels)))
    as.numeric(ifelse(is.na(tab[yes_level]), 0, tab[yes_level]))
  })
}

stopCluster(cl)

# label cols
colnames(boot_mat) <- paste0(combos$imd, "___", combos$v)

# summarize: mean + 95% bootstrap CI
summary_list <- apply(boot_mat, 2, function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(c(mean = NA, lower = NA, upper = NA))
  c(mean  = mean(x),
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

summary_df <- as.data.frame(t(summary_list), stringsAsFactors = FALSE) %>%
  rownames_to_column("combo") %>%
  separate(combo, into = c("imd", "vehicle"), sep = "___")


summary_plot_df <- summary_df %>%
  mutate(across(c(mean, `lower.2.5%`, `upper.97.5%`), as.numeric))

colnames(summary_plot_df)[c(4,5)] <- c('lower', 'upper')

ggplot(summary_plot_df, aes(x = imd, y = mean, color = imd)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_wrap(~vehicle, scales = "free_y",
             labeller = labeller(vehicle = 
                                   c("bicycle_present" = "Bicycle",
                                     "car_present" = "Car",
                                     "bus_truck_present" = "Bus/Truck",
                                     "motorcycle_present" = "Motorcycle",
                                     "other_present" = "Other")
             )) +
  labs(x = "IMD Level", y = "Proportion of Accidents",
       title = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
ggsave("accident_type_vehicle.png", width = 8, height = 5, dpi = 300)


summary_plot_df
