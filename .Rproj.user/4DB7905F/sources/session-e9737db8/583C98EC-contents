# ==============================================================================
# 01d_prepare_onet_tasks.R
# Build O*NET task measures mapped to occ1990dd occupations
# Following Acemoglu & Autor (2011) variable mapping
# Chain: O*NET SOC → Census OCC2010 → occ1990dd (Dorn A8)
# ==============================================================================

onet_path <- file.path(DATA_RAW, "05_onet")

# ==============================================================================
# STEP 1: Extract O*NET task components
# ==============================================================================

skills <- read.delim(file.path(onet_path, "Skills.txt"))
wa <- read.delim(file.path(onet_path, "Work Activities.txt"))
ab <- read.delim(file.path(onet_path, "Abilities.txt"))
wc <- read.delim(file.path(onet_path, "Work Context.txt"))

nr_analytic_1 <- skills %>% filter(Element.ID == "2.A.1.e", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, mathematics = Data.Value)
nr_analytic_2 <- skills %>% filter(Element.ID == "2.B.2.i", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, complex_problem = Data.Value)

nr_interactive_1 <- wa %>% filter(Element.ID == "4.A.4.a.2", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, communicating = Data.Value)
nr_interactive_2 <- wa %>% filter(Element.ID == "4.A.4.b.1", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, coordinating = Data.Value)

nr_manual_1 <- wa %>% filter(Element.ID == "4.A.3.a.1", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, physical = Data.Value)
nr_manual_2 <- wa %>% filter(Element.ID == "4.A.3.a.2", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, handling = Data.Value)

r_cognitive <- wc %>% filter(Element.ID == "4.C.3.b.7", Scale.ID == "CX") %>%
  select(soc = O.NET.SOC.Code, repeating = Data.Value)

r_manual_1 <- ab %>% filter(Element.ID == "1.A.2.a.2", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, manual_dex = Data.Value)
r_manual_2 <- ab %>% filter(Element.ID == "1.A.2.a.3", Scale.ID == "IM") %>%
  select(soc = O.NET.SOC.Code, finger_dex = Data.Value)

onet_tasks <- nr_analytic_1 %>%
  left_join(nr_analytic_2, by = "soc") %>%
  left_join(nr_interactive_1, by = "soc") %>%
  left_join(nr_interactive_2, by = "soc") %>%
  left_join(nr_manual_1, by = "soc") %>%
  left_join(nr_manual_2, by = "soc") %>%
  left_join(r_cognitive, by = "soc") %>%
  left_join(r_manual_1, by = "soc") %>%
  left_join(r_manual_2, by = "soc") %>%
  mutate(
    GED_MATH = (mathematics + complex_problem) / 2,
    DCP      = (communicating + coordinating) / 2,
    STS      = repeating,
    FINGDEX  = (manual_dex + finger_dex) / 2,
    EYEHAND  = (physical + handling) / 2,
    soc_6digit = substr(soc, 1, 7)
  )

cat("O*NET tasks by detailed SOC:", nrow(onet_tasks), "\n")

# ==============================================================================
# STEP 2: Aggregate to 6-digit SOC level
# ==============================================================================

task_vars <- unname(unlist(DOT_TASK_VARS))

onet_by_soc6 <- onet_tasks %>%
  group_by(soc_6digit) %>%
  summarise(across(all_of(task_vars), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

cat("O*NET tasks by 6-digit SOC:", nrow(onet_by_soc6), "\n")

# ==============================================================================
# STEP 3: Build SOC → Census OCC2010 crosswalk
# ==============================================================================

soc_census <- read_excel(file.path(DATA_RAW, "06_crosswalks",
                                   "Census_Occupation_Code_2010",
                                   "2010-occ-codes-with-crosswalk-from-2002-2011.xls"),
                         skip = 7, col_names = FALSE)

soc_to_occ2010 <- soc_census %>%
  select(occ2010 = ...3, soc = ...4) %>%
  filter(!is.na(occ2010), !is.na(soc),
         grepl("^\\d{4}$", occ2010),
         grepl("^\\d{2}-", soc)) %>%
  mutate(occ2010 = as.numeric(occ2010),
         soc_6digit = substr(soc, 1, 7))

# ==============================================================================
# STEP 4: Map O*NET to OCC2010 (progressive matching)
# ==============================================================================

# Direct 6-digit match
matched_direct <- onet_by_soc6 %>%
  inner_join(soc_to_occ2010 %>% select(soc_6digit, occ2010),
             by = "soc_6digit")

# 5-digit fallback for unmatched
unmatched_soc <- onet_by_soc6 %>%
  filter(!soc_6digit %in% soc_to_occ2010$soc_6digit) %>%
  mutate(soc_5digit = substr(soc_6digit, 1, 6))

census_5digit <- soc_to_occ2010 %>%
  mutate(soc_5digit = substr(soc_6digit, 1, 6)) %>%
  group_by(soc_5digit) %>%
  summarise(occ2010 = first(occ2010), .groups = "drop")

matched_5digit <- unmatched_soc %>%
  inner_join(census_5digit, by = "soc_5digit") %>%
  select(soc_6digit, all_of(task_vars), occ2010)

onet_with_occ2010 <- bind_rows(matched_direct, matched_5digit)

cat("\nMatched direct:", nrow(matched_direct), "\n")
cat("Matched via 5-digit:", nrow(matched_5digit), "\n")
cat("Total matched:", nrow(onet_with_occ2010), "\n")

# Aggregate to OCC2010
onet_by_occ2010 <- onet_with_occ2010 %>%
  group_by(occ2010) %>%
  summarise(across(all_of(task_vars), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

# ==============================================================================
# STEP 5: Map to occ1990dd via Dorn A8
# ==============================================================================

a8 <- read_dta(file.path(DATA_RAW, "06_crosswalks", "Dorn_A8",
                         "a8_occ2010_occ1990dd.dta"))

onet_by_occ1990dd <- onet_by_occ2010 %>%
  inner_join(a8, by = c("occ2010" = "occ")) %>%
  group_by(occ1990dd) %>%
  summarise(across(all_of(task_vars), \(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

cat("O*NET tasks by occ1990dd:", nrow(onet_by_occ1990dd), "\n")
summary(onet_by_occ1990dd)

# ==============================================================================
# SAVE
# ==============================================================================

saveRDS(onet_by_occ1990dd, file.path(DATA_INT, "onet_task_means_occ1990dd.rds"))
cat("\nSaved onet_task_means_occ1990dd.rds\n")
