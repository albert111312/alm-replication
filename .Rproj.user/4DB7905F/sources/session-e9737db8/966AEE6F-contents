# ==============================================================================
# 01b_prepare_dot_tasks.R
# Build DOT task measures by occupation code from two sources:
#   1. ICPSR 7845 Part I → DOT by 1970 Census occ code x sex
#      (used for Census 1960, 1970 and MORG 1980 which have occ70)
#   2. ICPSR 08942/Treiman file → DOT by 1980 Census occ code
#      (used for Census 1980, 1990 and MORG 1990, 1998 which have occ80)
#   GED-MATH is not in ICPSR 08942, so bridge it from Part I
#   through occ1950.
# ==============================================================================

# ==============================================================================
# DOT by 1970 Census occupation x sex (from ICPSR 7845 Part I)
# ==============================================================================

dot1 <- read_dta(file.path(DATA_RAW, "04_dot", "07845-0001-Data.dta"))

# Create binary temperament indicators
dot1 <- dot1 %>%
  mutate(
    DCP = as.integer(V097 == "D" | V098 == "D" | V099 == "D" |
                       V100 == "D" | V101 == "D"),
    STS = as.integer(V097 == "T" | V098 == "T" | V099 == "T" |
                       V100 == "T" | V101 == "T")
  )

# Compute weighted means by occ1970 x sex
dot_by_occ70 <- dot1 %>%
  filter(!is.na(V083)) %>%
  rename(occ70 = V018, sex = V023, wt = V029,
         GED_MATH = V083, FINGDEX = V093, EYEHAND = V095) %>%
  mutate(DCP = DCP * 100, STS = STS * 100) %>%
  group_by(occ70, sex) %>%
  summarise(
    GED_MATH = weighted.mean(GED_MATH, wt, na.rm = TRUE),
    FINGDEX  = weighted.mean(FINGDEX, wt, na.rm = TRUE),
    EYEHAND  = weighted.mean(EYEHAND, wt, na.rm = TRUE),
    DCP      = weighted.mean(DCP, wt, na.rm = TRUE),
    STS      = weighted.mean(STS, wt, na.rm = TRUE),
    .groups = "drop"
  )

# Invert DOT aptitude scale so that higher values mean more routine manual input.
# Original DOT: 1 = high aptitude required (little routine), 5 = low aptitude required (more routine).
dot_by_occ70 <- dot_by_occ70 %>%
  mutate(
    FINGDEX = 6 - FINGDEX,
    EYEHAND = 6 - EYEHAND
  )

# Fill missing gender cells within occupations
dot_by_occ70 <- dot_by_occ70 %>%
  complete(occ70, sex = c(1, 2)) %>%
  group_by(occ70) %>%
  fill(GED_MATH, FINGDEX, EYEHAND, DCP, STS, .direction = "downup") %>%
  ungroup()

cat("=== DOT by occ70 ===\n")
cat("Unique occupations:", n_distinct(dot_by_occ70$occ70), "\n")
cat("Rows:", nrow(dot_by_occ70), "\n")

# ==============================================================================
# DOT by 1980 Census occupation (from ICPSR 08942 + GED-MATH bridge)
# ==============================================================================

# --- Load ICPSR 08942/Treiman file: 4 of 5 task variables by occ80 ---
treiman <- read_dta(file.path(DATA_RAW, "06_crosswalks", "ICPSR_08942_Treiman",
                              "08942-0001-Data.dta"))

dot_treiman <- treiman %>%
  select(occ80 = OC80, DCP, STS, FINGDEX = FNGRDXT, EYEHAND = EYHNFTC) %>%
  filter(!is.na(DCP)) %>%  # drop rows with missing values
  mutate(
    FINGDEX = 6 - FINGDEX,
    EYEHAND = 6 - EYEHAND
  )

# --- Bridge GED-MATH from occ70 → occ1950 → occ80 ---
# Step 1: GED-MATH means by occ1950 from 1970 Census
c70 <- census %>% filter(year == 1970, empstat == 1, age >= AGE_MIN, age <= AGE_MAX)

gedmath_by_occ1950 <- c70 %>%
  left_join(dot_by_occ70 %>% select(occ70, sex, GED_MATH),
            by = c("occ" = "occ70", "sex")) %>%
  filter(!is.na(GED_MATH)) %>%
  group_by(occ1950) %>%
  summarise(GED_MATH = weighted.mean(GED_MATH, perwt), .groups = "drop")

# Step 2: Modal occ1950 for each occ80 from 1980 Census
c80 <- census %>% filter(year == 1980, empstat == 1, age >= AGE_MIN, age <= AGE_MAX)

occ80_to_occ1950 <- c80 %>%
  group_by(occ) %>%
  summarise(occ1950 = occ1950[which.max(perwt)], .groups = "drop") %>%
  rename(occ80 = occ)

# Step 3: Chain to get GED-MATH by occ80
gedmath_by_occ80 <- occ80_to_occ1950 %>%
  left_join(gedmath_by_occ1950, by = "occ1950") %>%
  select(occ80, GED_MATH)

# --- Combine Treiman + GED-MATH into full DOT by occ80 ---
dot_by_occ80 <- dot_treiman %>%
  left_join(gedmath_by_occ80, by = "occ80")

cat("\n=== DOT by occ80 ===\n")
cat("Unique occupations:", n_distinct(dot_by_occ80$occ80), "\n")
cat("Missing GED-MATH:", sum(is.na(dot_by_occ80$GED_MATH)), "\n")
summary(dot_by_occ80)

# ==============================================================================
# SAVE
# ==============================================================================

saveRDS(dot_by_occ70, file.path(DATA_INT, "dot_task_means_occ70.rds"))
saveRDS(dot_by_occ80, file.path(DATA_INT, "dot_task_means_occ80.rds"))
cat("\nSaved dot_task_means_occ70.rds and dot_task_means_occ80.rds\n")
