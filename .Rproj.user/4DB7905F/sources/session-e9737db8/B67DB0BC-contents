# ==============================================================================
# 03a_compute_decade_changes.R
# Compute within-industry decadal task changes (10 x annual change)
# ==============================================================================

industry_pctile <- readRDS(file.path(DATA_INT, "industry_task_percentiles.rds"))

# --- Define decade pairs with correct data source for each endpoint ---
decades <- ALM_DECADES

task_vars <- unname(unlist(DOT_TASK_VARS))

task_changes <- list()

for (i in seq_len(nrow(decades))) {
  d <- decades[i, ]
  
  t0 <- industry_pctile %>%
    filter(year == d$year0, source == d$source0) %>%
    select(ind1990dd, all_of(task_vars)) %>%
    rename_with(~ paste0(.x, "_t0"), all_of(task_vars))
  
  t1 <- industry_pctile %>%
    filter(year == d$year1, source == d$source1) %>%
    select(ind1990dd, all_of(task_vars)) %>%
    rename_with(~ paste0(.x, "_t1"), all_of(task_vars))
  
  merged <- inner_join(t0, t1, by = "ind1990dd")
  
  n_years <- d$year1 - d$year0
  
  for (task in task_vars) {
    merged[[paste0("d_", task)]] <- 10 * (merged[[paste0(task, "_t1")]] - 
                                            merged[[paste0(task, "_t0")]]) / n_years
  }
  
  merged$decade <- d$decade
  
  task_changes[[i]] <- merged %>%
    select(ind1990dd, decade, starts_with("d_"))
}

task_changes_df <- bind_rows(task_changes)

cat("=== Decade Changes ===\n")
task_changes_df %>% count(decade) %>% print()

# Verify direction of trends
cat("\n=== Unweighted mean decadal task changes ===\n")
task_changes_df %>%
  group_by(decade) %>%
  summarise(across(starts_with("d_"), mean)) %>%
  print(width = 100)

saveRDS(task_changes_df, file.path(DATA_INT, "industry_task_changes.rds"))
cat("\nSaved industry_task_changes.rds\n")
