# ==============================================================================
# 06a_merge_final_dataset.R
# Merge task changes, employment weights, and computer use into
# regression-ready dataset
# ==============================================================================

task_changes <- readRDS(file.path(DATA_INT, "industry_task_changes.rds"))
emp_weights <- readRDS(file.path(DATA_INT, "industry_weights.rds"))
comp_dd <- readRDS(file.path(DATA_INT, "industry_computer_use_dd.rds"))

# --- Merge task changes with employment weights ---
reg_data <- task_changes %>%
  left_join(emp_weights, by = c("ind1990dd", "decade"))

# --- Merge computer use ---
reg_data <- reg_data %>%
  left_join(comp_dd %>% select(ind1990dd, delta_computer), by = "ind1990dd")

# --- Verify ---
cat("=== Final Regression Dataset ===\n")
reg_data %>% count(decade) %>% print()

cat("\nMissing values:\n")
cat("  emp_weight NA:", sum(is.na(reg_data$emp_weight)), "\n")
cat("  delta_computer NA:", sum(is.na(reg_data$delta_computer)), "\n")

cat("\nWeighted mean delta_computer by decade:\n")
reg_data %>%
  group_by(decade) %>%
  summarise(
    wmean_delta_comp = weighted.mean(delta_computer, emp_weight, na.rm = TRUE),
    .groups = "drop"
  ) %>% print()

saveRDS(reg_data, file.path(DATA_FINAL, "tableIII_data.rds"))
cat("\nSaved tableIII_data.rds\n")