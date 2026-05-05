# ==============================================================================
# 05a_build_employment_weights.R
# Build industry employment weights as mean FTE share over decade endpoints
# ==============================================================================

industry_pctile <- readRDS(file.path(DATA_INT, "industry_task_percentiles.rds"))

# --- Define decade pairs (must match 03a) ---
decades <- ALM_DECADES

emp_weights <- list()

for (i in seq_len(nrow(decades))) {
  d <- decades[i, ]
  
  fte_t0 <- industry_pctile %>%
    filter(year == d$year0, source == d$source0) %>%
    select(ind1990dd, fte_t0 = total_fte)
  
  fte_t1 <- industry_pctile %>%
    filter(year == d$year1, source == d$source1) %>%
    select(ind1990dd, fte_t1 = total_fte)
  
  merged <- inner_join(fte_t0, fte_t1, by = "ind1990dd")
  
  # Weight = mean FTE share over the two endpoints
  total_fte_mean <- (sum(merged$fte_t0) + sum(merged$fte_t1)) / 2
  merged$emp_weight <- ((merged$fte_t0 + merged$fte_t1) / 2) / total_fte_mean
  
  merged$decade <- d$decade
  
  emp_weights[[i]] <- merged %>%
    select(ind1990dd, decade, emp_weight)
}

emp_weights_df <- bind_rows(emp_weights)

cat("=== Employment Weights ===\n")
emp_weights_df %>%
  group_by(decade) %>%
  summarise(
    n = n(),
    sum_weights = sum(emp_weight),
    .groups = "drop"
  ) %>% print()

saveRDS(emp_weights_df, file.path(DATA_INT, "industry_weights.rds"))
cat("\nSaved industry_weights.rds\n")