# ==============================================================================
# 04a_build_computer_use.R
# Build industry-level computer use measure from CPS October Supplements
# ==============================================================================

computer_use_raw <- read_dta(file.path(DATA_RAW, "03_cps_computer", "cps_computer_use_all.dta"))

# Filter to employed workers 18-64 with valid computer use response
computer_use_clean <- computer_use_raw %>%
  filter(year %in% c(1984, 1997),
         empstat %in% c(10, 12),
         age >= AGE_MIN, age <= AGE_MAX,
         ciwrkcmp %in% c(1, 2),
         ind1990 > 0) %>%
  mutate(uses_computer = as.integer(ciwrkcmp == 2))

# Compute weighted share of computer use by industry and year
computer_use_by_ind <- computer_use_clean %>%
  group_by(ind1990, year) %>%
  summarise(
    computer_share = weighted.mean(uses_computer, wtfinl, na.rm = TRUE),
    n_workers = n(),
    .groups = "drop"
  )

# Pivot to wide and compute total change
computer_use_wide <- computer_use_by_ind %>%
  pivot_wider(id_cols = ind1990, 
              names_from = year, 
              values_from = c(computer_share, n_workers),
              names_sep = "_")

computer_use_change <- computer_use_wide %>%
  filter(!is.na(computer_share_1984), !is.na(computer_share_1997)) %>%
  mutate(
    delta_computer = computer_share_1997 - computer_share_1984
  )

# Summary
cat("=== Computer Use Summary (by ind1990) ===\n")
cat("Industries with both years:", nrow(computer_use_change), "\n")
cat("Mean computer share 1984:", mean(computer_use_change$computer_share_1984, na.rm = TRUE), "\n")
cat("Mean computer share 1997:", mean(computer_use_change$computer_share_1997, na.rm = TRUE), "\n")
cat("Mean delta_computer:", mean(computer_use_change$delta_computer, na.rm = TRUE), "\n")

# ==============================================================================
# Aggregate to ind1990dd consistent industries
# ==============================================================================

ind1990dd_crosswalk <- readRDS(file.path(DATA_INT, "ind1990dd_crosswalk.rds"))

comp_dd <- computer_use_change %>%
  mutate(ind1990dd = case_when(
    ind1990 %in% ind1990dd_crosswalk$ind1990 ~ 
      ind1990dd_crosswalk$ind1990dd[match(ind1990, ind1990dd_crosswalk$ind1990)],
    TRUE ~ ind1990
  )) %>%
  group_by(ind1990dd) %>%
  summarise(
    computer_share_1984 = weighted.mean(computer_share_1984, n_workers_1984, na.rm = TRUE),
    computer_share_1997 = weighted.mean(computer_share_1997, n_workers_1997, na.rm = TRUE),
    delta_computer = weighted.mean(delta_computer, 
                                   (n_workers_1984 + n_workers_1997) / 2, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Computer Use Summary (by ind1990dd) ===\n")
cat("Industries:", nrow(comp_dd), "\n")
cat("Mean delta_computer:", mean(comp_dd$delta_computer, na.rm = TRUE), "\n")

saveRDS(comp_dd, file.path(DATA_INT, "industry_computer_use_dd.rds"))
cat("Saved industry_computer_use_dd.rds\n")