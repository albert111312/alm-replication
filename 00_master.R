# ==============================================================================
# 00_master.R
# Master script to run the entire ALM replication pipeline
# ==============================================================================

# Clear environment
rm(list = ls())
gc()

# Load configuration
source("01_config.R")

# ==============================================================================
# STEP 0: Data Preparation
# ==============================================================================
cat("\n========== STEP 0: Data Preparation ==========\n")

source("01_code/01_data_preparation/01a_clean_cps_census_replication.R")
source("01_code/01_data_preparation/01b_prepare_dot_tasks.R")
source("01_code/01_data_preparation/01c_build_crosswalks.R")
source("01_code/01_data_preparation/01d_prepare_onet_tasks.R")
source("01_code/01_data_preparation/01e_clean_cps_extension.R")

# ==============================================================================
# STEP 1: Build Industry-Year Task Levels
# ==============================================================================
cat("\n========== STEP 1: Industry-Year Task Levels ==========\n")

source("01_code/02_step1_industry_task_levels/02a_merge_tasks_to_workers.R")
source("01_code/02_step1_industry_task_levels/02b_aggregate_to_industry_year.R")
source("01_code/02_step1_industry_task_levels/02c_convert_to_percentiles.R")

# ==============================================================================
# STEP 2: Compute Decade Changes in Task Input
# ==============================================================================
cat("\n========== STEP 2: Decade Task Changes ==========\n")

source("01_code/03_step2_task_changes/03a_compute_decade_changes.R")

# ==============================================================================
# STEP 3: Build Computerization Regressor
# ==============================================================================
cat("\n========== STEP 3: Computerization Measure ==========\n")

source("01_code/04_step3_computerization/04a_build_computer_use.R")

# ==============================================================================
# STEP 4: Build Employment Weights
# ==============================================================================
cat("\n========== STEP 4: Employment Weights ==========\n")

source("01_code/05_step4_weights/05a_build_employment_weights.R")

# ==============================================================================
# STEP 5: Merge and Run Regressions
# ==============================================================================
cat("\n========== STEP 5: Table III Regressions ==========\n")

source("01_code/06_step5_regressions/06a_merge_final_dataset.R")
source("01_code/06_step5_regressions/06b_run_tableIII_regressions_original.R")
source("01_code/06_step5_regressions/06c_run_tableIII_regressions_extension.R")
source("01_code/06_step5_regressions/06d_generate_tables.R")
source("01_code/06_step5_regressions/06e_generate_figures.R")

cat("\n========== PIPELINE COMPLETE ==========\n")