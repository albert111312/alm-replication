# ==============================================================================
# 01a_clean_cps_census_replication.R
# Clean and standardize Census and NBER CPS MORG samples for ALM replication
# Carries through original occupation codes for direct DOT matching
# =============================================================================

# ==============================================================================
# 1. CENSUS (1960, 1970, 1980, 1990) from IPUMS USA
# ==============================================================================

census <- read_dta(file.path(DATA_RAW, "01_census", "census_1960_70_80_90.dta"))

hrs_midpoints <- c(`0` = 0, `1` = 7.5, `2` = 22, `3` = 32, 
                   `4` = 37, `5` = 40, `6` = 44.5, `7` = 54, `8` = 70)

wks_midpoints <- c(`0` = 0, `1` = 7, `2` = 20, `3` = 33, 
                   `4` = 43.5, `5` = 48.5, `6` = 51)

census_clean <- census %>%
  filter(empstat == 1, age >= AGE_MIN, age <= AGE_MAX) %>%
  mutate(
    hours = case_when(
      !is.na(hrswork1) ~ as.numeric(hrswork1),
      TRUE ~ hrs_midpoints[as.character(hrswork2)]
    ),
    weeks = case_when(
      !is.na(wkswork1) ~ as.numeric(wkswork1),
      TRUE ~ wks_midpoints[as.character(wkswork2)]
    ),
    fte_wt = perwt * (hours / 35) * weeks,
    source = "census",
    # Identify which occupation coding system applies
    # Census 1960: 1960 codes; 1970: 1970 codes; 1980/1990: 1980 codes
    occ_system = case_when(
      year == 1960 ~ "occ60",
      year == 1970 ~ "occ70",
      year %in% c(1980, 1990) ~ "occ80"
    ),
    educ_group = case_when(
      educ <= 5  ~ 1L,
      educ == 6  ~ 2L,
      educ <= 9  ~ 3L,
      educ >= 10 ~ 4L
    )
  ) %>%
  select(year, source, sex, age, educ_raw = educ, educ_group,
         occ, occ1950, occ_system, ind1990,
         hours, weeks, wt = perwt, fte_wt)

cat("=== Census Summary (Data Quality Check) ===\n")
census_clean %>% group_by(year, occ_system) %>%
  summarise(n = n(), 
            mean_hours = mean(hours, na.rm = TRUE),
            pct_fte_valid = 100 * mean(fte_wt > 0, na.rm = TRUE),
            .groups = "drop") %>% print()

# ==============================================================================
# 2. NBER MORG (1980, 1990, 1998)
# ==============================================================================

# --- Build industry crosswalks from IPUMS Census ---
# 1970 IND → IND1990 (for MORG 1980)
ind70_to_ind1990 <- census %>%
  filter(year == 1970, empstat == 1, age >= AGE_MIN, age <= AGE_MAX) %>%
  group_by(ind) %>%
  summarise(ind1990 = ind1990[which.max(perwt)], .groups = "drop") %>%
  rename(ind70 = ind)

# 1980 IND → IND1990 (for MORG 1990/1998)
ind80_to_ind1990 <- census %>%
  filter(year == 1980, empstat == 1, age >= AGE_MIN, age <= AGE_MAX) %>%
  group_by(ind) %>%
  summarise(ind1990 = ind1990[which.max(perwt)], .groups = "drop") %>%
  rename(ind80 = ind)

# --- Clean MORG 1980 ---
morg80 <- read_dta(file.path(DATA_RAW, "02_cps_morg", "morg80.dta"))

morg80_clean <- morg80 %>%
  filter(esr %in% c(1, 2), age >= AGE_MIN, age <= AGE_MAX,
         !is.na(occ70), !is.na(hourslw), hourslw > 0) %>%
  left_join(ind70_to_ind1990, by = "ind70") %>%
  filter(!is.na(ind1990)) %>%
  mutate(
    year = 1980, source = "cps", occ_system = "occ70",
    occ = occ70,  # carry through original occ code
    hours = hourslw, weeks = NA_real_,
    fte_wt = weight * (hours / 35),
    educ_group = case_when(
      gradeat <= 11 ~ 1L,
      gradeat == 12 ~ 2L,
      gradeat <= 15 ~ 3L,
      gradeat >= 16 ~ 4L
    ),
    occ1950 = NA_real_
  ) %>%
  select(year, source, sex, age, educ_raw = gradeat, educ_group,
         occ, occ1950, occ_system, ind1990,
         hours, weeks, wt = weight, fte_wt)

# --- Clean MORG 1990 ---
morg90 <- read_dta(file.path(DATA_RAW, "02_cps_morg", "morg90.dta"))

morg90_clean <- morg90 %>%
  filter(doinglw %in% c(1, 2), age >= AGE_MIN, age <= AGE_MAX,
         !is.na(occ80), !is.na(hourslw), hourslw > 0) %>%
  left_join(ind80_to_ind1990, by = "ind80") %>%
  filter(!is.na(ind1990)) %>%
  mutate(
    year = 1990, source = "cps", occ_system = "occ80",
    occ = occ80,
    hours = hourslw, weeks = NA_real_,
    fte_wt = weight * (hours / 35),
    educ_group = case_when(
      gradeat <= 11 ~ 1L,
      gradeat == 12 ~ 2L,
      gradeat <= 15 ~ 3L,
      gradeat >= 16 ~ 4L
    ),
    occ1950 = NA_real_
  ) %>%
  select(year, source, sex, age, educ_raw = gradeat, educ_group,
         occ, occ1950, occ_system, ind1990,
         hours, weeks, wt = weight, fte_wt)

# --- Clean MORG 1998 ---
morg98 <- read_dta(file.path(DATA_RAW, "02_cps_morg", "morg98.dta"))

morg98_clean <- morg98 %>%
  filter(lfsr94 %in% c(1, 2), age >= AGE_MIN, age <= AGE_MAX,
         !is.na(occ80), !is.na(hourslw), hourslw > 0) %>%
  left_join(ind80_to_ind1990, by = "ind80") %>%
  filter(!is.na(ind1990)) %>%
  mutate(
    year = 1998, source = "cps", occ_system = "occ80",
    occ = occ80,
    hours = hourslw, weeks = NA_real_,
    fte_wt = weight * (hours / 35),
    educ_group = case_when(
      grade92 <= 38 ~ 1L,
      grade92 <= 39 ~ 2L,
      grade92 <= 42 ~ 3L,
      grade92 >= 43 ~ 4L
    ),
    occ1950 = NA_real_
  ) %>%
  select(year, source, sex, age, educ_raw = grade92, educ_group,
         occ, occ1950, occ_system, ind1990,
         hours, weeks, wt = weight, fte_wt)

# ==============================================================================
# 3. STACK AND SAVE
# ==============================================================================

cps_all <- bind_rows(morg80_clean, morg90_clean, morg98_clean)

cat("\n=== CPS MORG Summary ===\n")
cps_all %>% group_by(year, occ_system) %>%
  summarise(n = n(), mean_hours = mean(hours), .groups = "drop") %>% print()

workers <- bind_rows(census_clean, cps_all)

cat("\n=== Final Dataset ===\n")
workers %>% count(year, source, occ_system) %>% print()

saveRDS(workers, file.path(DATA_INT, "workers_clean.rds"))
cat("\nSaved:", nrow(workers), "worker observations\n")
