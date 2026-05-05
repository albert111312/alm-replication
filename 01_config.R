# ==============================================================================
# config.R
# Global parameters, paths, and settings for ALM replication/extension
# ==============================================================================

# --- Paths ---
ROOT <- here::here()
DATA_RAW <- file.path(ROOT, "00_data", "00_raw") # raw data
DATA_INT <- file.path(ROOT, "00_data", "01_intermediate") # intermediate data
DATA_FINAL <- file.path(ROOT, "00_data", "02_final") # final data
OUTPUT <- file.path(ROOT, "02_output")

# --- Packages ---
required_packages <- c(
  "tidyverse", "haven", "data.table", "fixest", "modelsummary",
  "ipumsr", "here", "janitor", "broom", "kableExtra", "readxl",
  "gt", "webshot2"
)
invisible(lapply(required_packages, library, character.only = TRUE))

# --- Set options ---
options(scipen = 999)
theme_set(theme_minimal())

# --- Time periods ---
# Original ALM decades
ALM_DECADES <- tribble(
  ~decade,       ~year0, ~source0,  ~year1, ~source1,
  "1960-1970",   1960,   "census",  1970,   "census",
  "1970-1980",   1970,   "census",  1980,   "census",
  "1980-1990",   1980,   "cps",     1990,   "cps",
  "1990-1998",   1990,   "cps",     1998,   "cps"
)

# Extension decades
EXTENSION_DECADES <- tribble(
  ~decade,       ~year0, ~source0,    ~year1, ~source1,
  "2000-2010",   2000,   "cps_ext",   2010,   "cps_ext",
  "2010-2020",   2010,   "cps_ext",   2020,   "cps_ext"
)

# --- Sample restrictions ---
AGE_MIN <- 18
AGE_MAX <- 64

# --- Task variable mappings ---
# DOT 1977 (for periods through ~2000)
DOT_TASK_VARS <- list(
  nonroutine_analytic    = "GED_MATH",
  nonroutine_interactive = "DCP",
  routine_cognitive      = "STS",
  routine_manual         = "FINGDEX",
  nonroutine_manual      = "EYEHAND"
)

# O*NET (for periods after 2000)
ONET_TASK_VARS <- list(
  nonroutine_analytic    = c("2.A.1.e", "2.B.2.i"),      # Math, Complex Problem Solving (Skills)
  nonroutine_interactive = c("4.A.4.a.2", "4.A.4.b.1"),  # Communicating, Coordinating (Work Activities)
  routine_cognitive      = "4.C.3.b.7",                  # Importance of Repeating Tasks (Work Context)
  routine_manual         = c("1.A.2.a.2", "1.A.2.a.3"),  # Manual Dexterity, Finger Dexterity (Abilities)
  nonroutine_manual      = c("4.A.3.a.1", "4.A.3.a.2")   # Physical Activities, Handling Objects (Work Activities)
)

# --- Shared utilities ---
to_percentile <- function(values, ref_values, ref_weights) {
  sapply(values, function(v) {
    100 * sum(ref_weights[ref_values <= v]) / sum(ref_weights)
  })
}
