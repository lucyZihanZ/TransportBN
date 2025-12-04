library(bnlearn)
library(dplyr)
library(foreach)
library(doParallel)

set.seed(123)
d_surface <- unique(data_bn$road_surface_conditions)

B <- 200               # number of bootstrap replicates (reduce if needed)
cp_n <- 2000           # cpquery samples per query (balance speed / precision)
cores <- parallel::detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)

results <- foreach(b = 1:B, .combine = rbind, .packages = "bnlearn") %dopar% {
  # resample rows
  print('hi')
  idx <- sample(nrow(data_bn), size = nrow(data_bn), replace = TRUE)
  data_b <- data_bn[idx, , drop = FALSE]
  
  # refit parameters (bayes) with fixed structure
  fitted_b <- bn.fit(dag, data_b, method = "bayes")
  
  # for each surface, compute p_T - p_R
  sapply(d_surface, function(surf) {
    p_T <- cpquery(fitted_b,
                   event    = (casualty_count_cat == "Fatal"),
                   evidence = list(junction_type = "T-Junction",
                                   road_surface_conditions = surf),
                   method   = "lw",
                   n = cp_n)
    p_R <- cpquery(fitted_b,
                   event    = (casualty_count_cat == "Fatal"),
                   evidence = list(junction_type = "Roundabout",
                                   road_surface_conditions = surf),
                   method   = "lw",
                   n = cp_n)
    (p_T - p_R) * 100
  })
}

stopCluster(cl)


colnames(results) <- d_surface

# Summarize
summary_df <- apply(results, 2, function(col) {
  c(mean = mean(col, na.rm = TRUE),
    sd   = sd(col, na.rm = TRUE),
    lower = quantile(col, 0.025, na.rm = TRUE),
    upper = quantile(col, 0.975, na.rm = TRUE))
}) %>% t() %>% as.data.frame() %>%
  tibble::rownames_to_column("road_surface_conditions")

print(summary_df)


# Ensure numeric columns
summary_plot_df <- summary_df %>%
  mutate(across(c(mean, `lower.2.5%`, `upper.97.5%`), as.numeric))

colnames(summary_plot_df)[c(4,5)] <- c('lower', 'upper')

ggplot(summary_plot_df, aes(x = road_surface_conditions, y = mean)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Road Surface Condition",
       y = "Proportion of Fatal Accidents",
       title = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fatal_roundabout.png", width = 8, height = 5, dpi = 300)



# levels to evaluate
d_light <- unique(data_bn$light_conditions)

# bootstrap / cpquery parameters (tune for speed vs precision)
B <- 200       # bootstrap replicates (reduce if very slow)
cp_n <- 2000   # cpquery sampling per query (bootstrap provides main uncertainty)
cores <- max(1, parallel::detectCores() - 1)



# parallel setup
cl <- makeCluster(cores)
registerDoParallel(cl)

# Run bootstrap: each row is one bootstrap replicate; each column is a light level
boot_mat <- foreach(b = 1:B, .combine = rbind, .packages = "bnlearn") %dopar% {
  # resample rows with replacement
  idx <- sample(nrow(data_bn), nrow(data_bn), replace = TRUE)
  data_b <- data_bn[idx, , drop = FALSE]
  
  # re-fit parameters (bayes) with fixed structure
  fitted_b <- bn.fit(dag, data_b, method = "bayes")
  
  # compute differences for every light level (returns numeric vector length = length(d_light))
  sapply(d_light, function(light_i) {
    p_T <- cpquery(
      fitted_b,
      event = (casualty_count_cat == "Fatal"),
      evidence = list(
        junction_type = "T-Junction",
        light_conditions = light_i,
        pedestrian_any = "yes"
      ),
      method = "lw",
      n = cp_n
    )
    
    p_R <- cpquery(
      fitted_b,
      event = (casualty_count_cat == "Fatal"),
      evidence = list(
        junction_type = "Roundabout",
        light_conditions = light_i,
        pedestrian_any = "yes"
      ),
      method = "lw",
      n = cp_n
    )
    
    (p_T - p_R) * 100  # percent-point difference
  })
}

stopCluster(cl)

# label matrix columns and convert to tidy summary
colnames(boot_mat) <- d_light

summary_df <- as.data.frame(t(apply(boot_mat, 2, function(x) {
  c(mean = mean(x, na.rm = TRUE),
    lower = quantile(x, 0.025, na.rm = TRUE),
    upper = quantile(x, 0.975, na.rm = TRUE))
})), stringsAsFactors = FALSE)

summary_df <- as.data.frame(t(apply(boot_mat, 2, function(x) {
  c(mean = mean(x, na.rm = TRUE),
    lower = quantile(x, 0.025, na.rm = TRUE),
    upper = quantile(x, 0.975, na.rm = TRUE))
})), stringsAsFactors = FALSE) %>%
  rownames_to_column("light_conditions") %>%
  mutate(
    mean = as.numeric(mean),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    light_conditions = factor(light_conditions,
                              levels = light_conditions[order(mean, decreasing = TRUE)])
  )

print(summary_df)

# plot
p <- ggplot(summary_df, aes(x = light_conditions, y = mean)) +
  geom_col(width = 0.6, fill = "steelblue", alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
  coord_flip() +
  labs(
    title = "T-Junction vs Roundabout: Difference in P(Fatal) by Light Conditions (pedestrians = yes)",
    subtitle = paste0("Difference = 100*(P(Fatal|T-Junction,light,ped=yes) - P(Fatal|Roundabout,light,ped=yes)); B=", B, ", cpquery n=", cp_n),
    x = "Light conditions",
    y = "Difference in probability (percentage points)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

print(p)

# Overall

B <- 200               # number of bootstrap replicates (reduce if needed)
cp_n <- 2000           # cpquery samples per query (balance speed / precision)
cores <- parallel::detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)

results <- foreach(b = 1:B, .combine = rbind, .packages = "bnlearn") %dopar% {
  # resample rows
  print('hi')
  idx <- sample(nrow(data_bn), size = nrow(data_bn), replace = TRUE)
  data_b <- data_bn[idx, , drop = FALSE]
  
  # refit parameters (bayes) with fixed structure
  fitted_b <- bn.fit(dag, data_b, method = "bayes")
  
  p_T <- cpquery(fitted_b,
                   event    = (casualty_count_cat == "Fatal"),
                   evidence = list(junction_type = "T-Junction"),
                   method   = "lw",
                   n = cp_n)
    p_R <- cpquery(fitted_b,
                   event    = (casualty_count_cat == "Fatal"),
                   evidence = list(junction_type = "Roundabout"),
                   method   = "lw",
                   n = cp_n)
    (p_T - p_R) * 100
}

stopCluster(cl)

# `results` will be a B x length(d_surface) matrix (rows = boot replicates)
colnames(results)

# Summarize
summary_df <- apply(results, 2, function(col) {
  c(mean = mean(col, na.rm = TRUE),
    sd   = sd(col, na.rm = TRUE),
    lower = quantile(col, 0.025, na.rm = TRUE),
    upper = quantile(col, 0.975, na.rm = TRUE))
})

summary_df

library(ggplot2)

results_df <- data.frame(effect = results)

# Histogram + density overlay
ggplot(results_df, aes(x = effect)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  geom_vline(aes(xintercept = mean(effect, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1) +
  labs(
    x = "Effect (% P(Fatal) T-Junction minus Roundabout)",
    y = "Density",
    title = ""
  ) +
  theme_minimal(base_size = 14)


ggsave("overall_roundabout.png", width = 8, height = 5, dpi = 300)
