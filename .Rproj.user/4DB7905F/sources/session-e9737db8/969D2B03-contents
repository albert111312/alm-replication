# ==============================================================================
# 01e_clean_cps_extension.R
# Clean IPUMS CPS Basic Monthly data for Part II extension (2000-2020)
# Filter to MORG respondents, assign occ1990dd and O*NET tasks
# ==============================================================================

cps_ext <- read_dta(file.path(DATA_RAW, "02_cps_morg",
                              "cps_basic_monthly_00_10_19_20.dta"))

a8 <- read_dta(file.path(DATA_RAW, "06_crosswalks", "Dorn_A8",
                         "a8_occ2010_occ1990dd.dta"))
onet_tasks <- readRDS(file.path(DATA_INT, "onet_task_means_occ1990dd.rds"))
ind1990dd_crosswalk <- readRDS(file.path(DATA_INT, "ind1990dd_crosswalk.rds"))

# Filter to MORG-eligible, employed, 18-64, valid hours
ext_clean <- cps_ext %>%
  filter(eligorg == 1,
         empstat %in% c(10, 12),
         age >= AGE_MIN, age <= AGE_MAX,
         !is.na(ahrsworkt), ahrsworkt < 999) %>%
  mutate(
    hours = ahrsworkt,
    fte_wt = wtfinl * (hours / 35),
    source = "cps_ext",
    educ_group = case_when(
      educ <= 72  ~ 1L,   # HS dropout
      educ <= 73  ~ 2L,   # HS graduate
      educ <= 92  ~ 3L,   # Some college
      educ >= 100 ~ 4L    # College+
    )
  )

# Map OCC2010 → occ1990dd
ext_clean <- ext_clean %>%
  left_join(a8, by = c("occ2010" = "occ"))

# Apply ind1990dd
ext_clean <- ext_clean %>%
  mutate(ind1990dd = case_when(
    ind1990 %in% ind1990dd_crosswalk$ind1990 ~
      ind1990dd_crosswalk$ind1990dd[match(ind1990, ind1990dd_crosswalk$ind1990)],
    TRUE ~ ind1990
  ))

# Merge O*NET tasks
ext_clean <- ext_clean %>%
  left_join(onet_tasks, by = "occ1990dd")

cat("=== Extension Match Rates ===\n")
ext_clean %>%
  group_by(year) %>%
  summarise(
    n = n(),
    pct_occ_matched = 100 * mean(!is.na(occ1990dd)),
    pct_task_matched = 100 * mean(!is.na(GED_MATH)),
    mean_hours = mean(hours),
    .groups = "drop"
  ) %>% print()

# Drop unmatched
ext_worker_tasks <- ext_clean %>%
  filter(!is.na(GED_MATH), !is.na(occ1990dd), ind1990dd > 0)

cat("\nFinal extension rows:", nrow(ext_worker_tasks), "\n")

saveRDS(ext_worker_tasks, file.path(DATA_INT, "worker_tasks_extension.rds"))
cat("Saved worker_tasks_extension.rds\n")
