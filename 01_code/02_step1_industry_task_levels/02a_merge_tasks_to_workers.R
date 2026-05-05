# ==============================================================================
# 02a_merge_tasks_to_workers.R
# Merge DOT task measures to workers using original occupation codes:
#   - Census 1960: Priebe-Greene crosswalk occ1960 → occ1970 → DOT (by sex)
#   - Census 1970, MORG 1980: occ70 x sex → dot_task_means_occ70
#   - Census 1980/1990, MORG 1990/1998: occ80 → dot_task_means_occ80 (Treiman)
# ==============================================================================

workers <- readRDS(file.path(DATA_INT, "workers_clean.rds"))
dot_occ70 <- readRDS(file.path(DATA_INT, "dot_task_means_occ70.rds"))
dot_occ80 <- readRDS(file.path(DATA_INT, "dot_task_means_occ80.rds"))
ind1990dd_crosswalk <- readRDS(file.path(DATA_INT, "ind1990dd_crosswalk.rds"))

task_vars <- unname(unlist(DOT_TASK_VARS))

# ==============================================================================
# BUILD DOT by occ1960 using Priebe-Greene crosswalk
# (Maps 1960 Census occ codes → 1970 occ codes → DOT task measures)
# ==============================================================================

pg_crosswalk <- read.csv(file.path(DATA_RAW, "06_crosswalks",
                                   "Priebe-Green_Crosswalk",
                                   "priebe-greene_crosswalk_1960_1970.csv"),
                         stringsAsFactors = FALSE)

# Males: weight DOT scores across 1970 destinations
pg_male <- pg_crosswalk %>%
  filter(male_pct_of_1960_in_1970 > 0) %>%
  select(occ1960, occ70 = occ1970, weight = male_pct_of_1960_in_1970) %>%
  left_join(dot_occ70 %>% filter(sex == 1), by = "occ70") %>%
  filter(!is.na(GED_MATH)) %>%
  group_by(occ1960) %>%
  summarise(
    across(all_of(task_vars), ~ weighted.mean(.x, weight)),
    .groups = "drop"
  ) %>%
  mutate(sex = 1)

# Females: weight DOT scores across 1970 destinations
pg_female <- pg_crosswalk %>%
  filter(female_pct_of_1960_in_1970 > 0) %>%
  select(occ1960, occ70 = occ1970, weight = female_pct_of_1960_in_1970) %>%
  left_join(dot_occ70 %>% filter(sex == 2), by = "occ70") %>%
  filter(!is.na(GED_MATH)) %>%
  group_by(occ1960) %>%
  summarise(
    across(all_of(task_vars), ~ weighted.mean(.x, weight)),
    .groups = "drop"
  ) %>%
  mutate(sex = 2)

dot_by_occ1960 <- bind_rows(pg_male, pg_female)

# Fill missing gender cells
dot_by_occ1960 <- dot_by_occ1960 %>%
  complete(occ1960, sex = c(1, 2)) %>%
  group_by(occ1960) %>%
  fill(all_of(task_vars), .direction = "downup") %>%
  ungroup()

cat("=== DOT by occ1960 (Priebe-Greene) ===\n")
cat("Unique 1960 occupations:", n_distinct(dot_by_occ1960$occ1960), "\n")
cat("Rows:", nrow(dot_by_occ1960), "\n\n")

# ==============================================================================
# MERGE BY OCCUPATION SYSTEM
# ==============================================================================

# Split workers by occupation coding system
w_occ60 <- workers %>% filter(occ_system == "occ60")
w_occ70 <- workers %>% filter(occ_system == "occ70")
w_occ80 <- workers %>% filter(occ_system == "occ80")

# --- Census 1960: merge via Priebe-Greene occ1960 x sex ---
w_occ60 <- w_occ60 %>%
  left_join(dot_by_occ1960, by = c("occ" = "occ1960", "sex"))

# --- Census 1970 and MORG 1980: merge via occ70 x sex ---
w_occ70 <- w_occ70 %>%
  left_join(dot_occ70, by = c("occ" = "occ70", "sex"))

# --- Census 1980/1990 and MORG 1990/1998: merge via occ80 ---
w_occ80 <- w_occ80 %>%
  left_join(dot_occ80, by = c("occ" = "occ80"))

# ==============================================================================
# RECOMBINE AND CHECK
# ==============================================================================

worker_tasks <- bind_rows(w_occ60, w_occ70, w_occ80)

cat("=== Match Rates ===\n")
worker_tasks %>%
  group_by(year, source, occ_system) %>%
  summarise(
    n = n(),
    pct_matched = 100 * mean(!is.na(GED_MATH)),
    .groups = "drop"
  ) %>% print()

# Drop unmatched and apply ind1990dd crosswalk
worker_tasks <- worker_tasks %>%
  filter(!is.na(GED_MATH)) %>%
  mutate(ind1990dd = case_when(
    ind1990 %in% ind1990dd_crosswalk$ind1990 ~
      ind1990dd_crosswalk$ind1990dd[match(ind1990, ind1990dd_crosswalk$ind1990)],
    TRUE ~ ind1990
  ))

cat("\nFinal rows:", nrow(worker_tasks), "\n")

saveRDS(worker_tasks, file.path(DATA_INT, "worker_tasks.rds"))
cat("Saved worker_tasks.rds\n")