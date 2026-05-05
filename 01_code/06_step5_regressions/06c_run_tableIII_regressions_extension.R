# ==============================================================================
# 06c_run_tableIII_extension.R
# Part II: Extend Table III regressions to 2000-2020
# ==============================================================================

ext_tasks <- readRDS(file.path(DATA_INT, "worker_tasks_extension.rds"))

task_vars <- unname(unlist(DOT_TASK_VARS))

# ==============================================================================
# Build computer use for extension decades
# ==============================================================================

comp_raw <- read_dta(file.path(DATA_RAW, "03_cps_computer", "cps_computer_use_all.dta"))
ind1990dd_crosswalk <- readRDS(file.path(DATA_INT, "ind1990dd_crosswalk.rds"))

# Build computer use for each pair of supplement years
build_computer_change <- function(data, year0, year1, crosswalk) {
  clean <- data %>%
    mutate(year = as.numeric(year)) %>%
    filter(year %in% c(year0, year1),
           empstat %in% c(10, 12),
           age >= AGE_MIN, age <= AGE_MAX,
           ciwrkcmp %in% c(1, 2),
           ind1990 > 0) %>%
    mutate(
      uses_computer = as.integer(ciwrkcmp == 2),
      ind1990dd = case_when(
        ind1990 %in% crosswalk$ind1990 ~
          crosswalk$ind1990dd[match(ind1990, crosswalk$ind1990)],
        TRUE ~ as.numeric(ind1990)
      )
    )
  
  by_ind_year <- clean %>%
    group_by(ind1990dd, year) %>%
    summarise(
      computer_share = weighted.mean(uses_computer, wtfinl, na.rm = TRUE),
      n_workers = n(),
      .groups = "drop"
    )
  
  wide <- by_ind_year %>%
    pivot_wider(id_cols = ind1990dd,
                names_from = year,
                values_from = c(computer_share, n_workers),
                names_sep = "_")
  
  names_y0 <- paste0("computer_share_", year0)
  names_y1 <- paste0("computer_share_", year1)
  
  wide %>%
    filter(!is.na(.data[[names_y0]]), !is.na(.data[[names_y1]])) %>%
    mutate(delta_computer = .data[[names_y1]] - .data[[names_y0]])
}

comp_ext <- build_computer_change(comp_raw, 1997, 2003, ind1990dd_crosswalk)

cat("=== Extension Computer Use (1997-2003) ===\n")
cat("Industries:", nrow(comp_ext), "\n")
cat("Mean delta_computer:", mean(comp_ext$delta_computer), "\n")

# ==============================================================================
# Aggregate extension data to industry-gender-education cells
# ==============================================================================

ext_cells_agg <- ext_tasks %>%
  filter(ind1990dd > 0, fte_wt > 0) %>%
  group_by(year, ind1990dd, sex, educ_group) %>%
  summarise(
    across(all_of(task_vars), \(x) weighted.mean(x, fte_wt, na.rm = TRUE)),
    cell_fte = sum(fte_wt),
    .groups = "drop"
  )

cat("Extension cells:", nrow(ext_cells_agg), "\n")

# ==============================================================================
# Convert to percentiles of 2000 O*NET task distribution
# ==============================================================================

ref_2000 <- ext_cells_agg %>% filter(year == 2000)
cat("2000 reference cells:", nrow(ref_2000), "\n")

ext_cells_pctile <- ext_cells_agg
for (task in task_vars) {
  ext_cells_pctile[[task]] <- to_percentile(
    ext_cells_agg[[task]],
    ref_2000[[task]],
    ref_2000$cell_fte
  )
}

# Aggregate to industry level
ext_industry_pctile <- ext_cells_pctile %>%
  group_by(year, ind1990dd) %>%
  summarise(
    across(all_of(task_vars), \(x) weighted.mean(x, cell_fte)),
    total_fte = sum(cell_fte),
    .groups = "drop"
  )

# Verify: 2000 means should be ~50
cat("\n=== Extension Percentile Verification ===\n")
ext_industry_pctile %>%
  group_by(year) %>%
  summarise(
    n = n(),
    across(all_of(task_vars), \(x) weighted.mean(x, total_fte)),
    .groups = "drop"
  ) %>% print(width = 100)

# ==============================================================================
# Compute decade changes
# ==============================================================================

# NOTE: 1990-2000 spans two measurement systems (DOT vs O*NET) and cannot
# be cleanly computed. We focus on 2000-2010 and 2010-2020.

ext_decades <- lapply(seq_len(nrow(EXTENSION_DECADES)), function(i) {
  d <- EXTENSION_DECADES[i, ]
  list(decade  = d$decade,
       t0_data = ext_industry_pctile %>% filter(year == d$year0),
       t0_year = d$year0,
       t1_data = ext_industry_pctile %>% filter(year == d$year1),
       t1_year = d$year1,
       comp    = comp_ext)
})

ext_reg_list <- list()

for (d in ext_decades) {
  t0 <- d$t0_data %>%
    select(ind1990dd, all_of(task_vars), total_fte) %>%
    rename_with(~ paste0(.x, "_t0"), all_of(task_vars)) %>%
    rename(fte_t0 = total_fte)
  
  t1 <- d$t1_data %>%
    select(ind1990dd, all_of(task_vars), total_fte) %>%
    rename_with(~ paste0(.x, "_t1"), all_of(task_vars)) %>%
    rename(fte_t1 = total_fte)
  
  merged <- inner_join(t0, t1, by = "ind1990dd")
  n_years <- d$t1_year - d$t0_year
  
  for (task in task_vars) {
    merged[[paste0("d_", task)]] <- 10 * (merged[[paste0(task, "_t1")]] -
                                            merged[[paste0(task, "_t0")]]) / n_years
  }
  
  total_fte_mean <- (sum(merged$fte_t0) + sum(merged$fte_t1)) / 2
  merged$emp_weight <- ((merged$fte_t0 + merged$fte_t1) / 2) / total_fte_mean
  
  merged <- merged %>%
    left_join(d$comp %>% select(ind1990dd, delta_computer), by = "ind1990dd") %>%
    filter(!is.na(delta_computer))
  
  merged$decade <- d$decade
  
  ext_reg_list[[d$decade]] <- merged %>%
    select(ind1990dd, decade, starts_with("d_"), emp_weight, delta_computer)
}

ext_reg_data <- bind_rows(ext_reg_list)

cat("=== Extension Regression Data ===\n")
ext_reg_data %>% count(decade) %>% print()

ext_reg_data %>%
  group_by(decade) %>%
  summarise(across(starts_with("d_"), \(x) weighted.mean(x, emp_weight))) %>%
  print(width = 100)

# ==============================================================================
# Run Part II Table III regressions
# ==============================================================================

# Load Part I results for comparison
part1_reg <- readRDS(file.path(DATA_FINAL, "tableIII_data.rds"))

# Combine Part I and Part II
all_reg <- bind_rows(part1_reg, ext_reg_data)

dep_vars <- c("d_GED_MATH", "d_DCP", "d_STS", "d_FINGDEX")
panel_labels <- c(
  "A. Nonroutine analytic",
  "B. Nonroutine interactive",
  "C. Routine cognitive",
  "D. Routine manual"
)

# All decades in order
all_decades <- c(rev(ALM_DECADES$decade), EXTENSION_DECADES$decade)
col_labels <- paste0(seq_along(all_decades), ". ", all_decades)

cat("\n====================================================\n")
cat("   TABLE III EXTENSION: PART I + PART II\n")
cat("   Dependent variable: 10 x annual within-industry\n")
cat("   change in task input (percentiles of 1960 distribution)\n")
cat("====================================================\n")

for (p in seq_along(dep_vars)) {
  cat(sprintf("\n--- Panel %s ---\n", panel_labels[p]))
  cat(sprintf("%-16s", ""))
  for (cl in col_labels) cat(sprintf("%12s", cl))
  cat("\n")

  coefs <- ses <- intercepts <- int_ses <- r2s <- wmeans <- c()
  
  for (dec in all_decades) {
    d <- all_reg %>% filter(decade == dec)
    
    if (nrow(d) == 0) {
      coefs <- c(coefs, NA); ses <- c(ses, NA)
      intercepts <- c(intercepts, NA); int_ses <- c(int_ses, NA)
      r2s <- c(r2s, NA); wmeans <- c(wmeans, NA)
      next
    }
    
    mod <- lm(as.formula(paste(dep_vars[p], "~ delta_computer")),
              data = d, weights = emp_weight)
    s <- summary(mod)
    
    coefs <- c(coefs, coef(mod)[2])
    ses <- c(ses, s$coefficients[2, 2])
    intercepts <- c(intercepts, coef(mod)[1])
    int_ses <- c(int_ses, s$coefficients[1, 2])
    r2s <- c(r2s, s$r.squared)
    wmeans <- c(wmeans, weighted.mean(d[[dep_vars[p]]], d$emp_weight))
  }
  
  cat(sprintf("%-16s", "Computer use"))
  for (v in coefs) cat(sprintf("%12.2f", v))
  cat("\n")
  
  cat(sprintf("%-16s", ""))
  for (v in ses) cat(sprintf("%12s", sprintf("(%.2f)", v)))
  cat("\n")
  
  cat(sprintf("%-16s", "Intercept"))
  for (v in intercepts) cat(sprintf("%12.2f", v))
  cat("\n")
  
  cat(sprintf("%-16s", ""))
  for (v in int_ses) cat(sprintf("%12s", sprintf("(%.2f)", v)))
  cat("\n")
  
  cat(sprintf("%-16s", "R-squared"))
  for (v in r2s) cat(sprintf("%12.2f", v))
  cat("\n")
  
  cat(sprintf("%-16s", "Weighted mean"))
  for (v in wmeans) cat(sprintf("%12.2f", v))
  cat("\n")
}

cat("\n")
ext_reg_data %>% 
  group_by(decade) %>% 
  summarise(n = n_distinct(ind1990dd)) %>% 
  print()

saveRDS(ext_reg_data, file.path(DATA_FINAL, "tableIII_extension_data.rds"))
cat("\nSaved tableIII_extension_data.rds\n")