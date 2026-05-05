# ==============================================================================
# 02c_convert_to_percentiles.R
# Convert task measures to percentiles of 1960 distribution, then aggregate 
# to industry level
# ==============================================================================

all_cells <- readRDS(file.path(DATA_INT, "industry_cells.rds"))

task_vars <- unname(unlist(DOT_TASK_VARS))

# --- Build 1960 reference distribution ---
ref_1960 <- all_cells %>% filter(year == 1960, source == "census")
cat("1960 reference cells:", nrow(ref_1960), "\n")

# --- Convert each cell's task value to its 1960 percentile ---
all_cells_pctile <- all_cells

for (task in task_vars) {
  all_cells_pctile[[task]] <- to_percentile(
    all_cells[[task]],
    ref_1960[[task]],
    ref_1960$cell_fte
  )
}

# Verify at cell level: 1960 weighted means should be ~50
cat("\n=== Cell-level verification (1960 should be ~50) ===\n")
all_cells_pctile %>%
  filter(year == 1960) %>%
  summarise(across(all_of(task_vars), ~ weighted.mean(.x, cell_fte))) %>%
  print()

# --- Aggregate percentiles to industry level ---
industry_pctile <- all_cells_pctile %>%
  group_by(year, source, ind1990dd) %>%
  summarise(
    across(all_of(task_vars), ~ weighted.mean(.x, cell_fte)),
    total_fte = sum(cell_fte),
    .groups = "drop"
  )

cat("\n=== Industry-level weighted mean percentiles ===\n")
industry_pctile %>%
  group_by(year, source) %>%
  summarise(across(all_of(task_vars), ~ weighted.mean(.x, total_fte)),
            .groups = "drop") %>%
  print(width = 100)

cat("\nIndustries per year:\n")
industry_pctile %>% count(year, source) %>% print()

saveRDS(industry_pctile, file.path(DATA_INT, "industry_task_percentiles.rds"))
cat("\nSaved industry_task_percentiles.rds\n")
