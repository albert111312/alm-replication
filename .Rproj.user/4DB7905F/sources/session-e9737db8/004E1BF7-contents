# ==============================================================================
# 06b_run_tableIII_regressions_original.R
# Replicate ALM Table III: Computerization and Industry Task Input
# ==============================================================================

reg_data <- readRDS(file.path(DATA_FINAL, "tableIII_data.rds"))

# --- Define regression parameters ---
dep_vars <- c("d_GED_MATH", "d_DCP", "d_STS", "d_FINGDEX")
panel_labels <- c(
  "A. Nonroutine analytic",
  "B. Nonroutine interactive",
  "C. Routine cognitive",
  "D. Routine manual"
)
decade_order <- rev(ALM_DECADES$decade)
col_labels <- paste0(seq_along(decade_order), ". ", decade_order)

# --- Store results ---
results <- list()

# --- Run regressions ---
cat("====================================================\n")
cat("   TABLE III: COMPUTERIZATION AND INDUSTRY TASK INPUT\n")
cat("   Dependent variable: 10 x annual within-industry\n")
cat("   change in task input (percentiles of 1960 distribution)\n")
cat("====================================================\n")

for (p in seq_along(dep_vars)) {
  cat(sprintf("\n--- Panel %s ---\n", panel_labels[p]))
  cat(sprintf("%-20s %12s %12s %12s %12s\n", "", 
              col_labels[1], col_labels[2], col_labels[3], col_labels[4]))
  
  panel_results <- list()
  
  for (j in seq_along(decade_order)) {
    dec <- decade_order[j]
    d <- reg_data %>% filter(decade == dec)
    
    mod <- lm(as.formula(paste(dep_vars[p], "~ delta_computer")),
              data = d, weights = emp_weight)
    s <- summary(mod)
    
    panel_results[[dec]] <- list(
      coef = coef(mod)[2],
      se = s$coefficients[2, 2],
      intercept = coef(mod)[1],
      int_se = s$coefficients[1, 2],
      r2 = s$r.squared,
      wmean = weighted.mean(d[[dep_vars[p]]], d$emp_weight)
    )
  }
  
  # Print coefficient
  cat(sprintf("%-20s %12.2f %12.2f %12.2f %12.2f\n", "Computer use",
              panel_results[[decade_order[1]]]$coef,
              panel_results[[decade_order[2]]]$coef,
              panel_results[[decade_order[3]]]$coef,
              panel_results[[decade_order[4]]]$coef))
  
  # Print SE
  cat(sprintf("%-20s %12s %12s %12s %12s\n", "1984-1997",
              sprintf("(%.2f)", panel_results[[decade_order[1]]]$se),
              sprintf("(%.2f)", panel_results[[decade_order[2]]]$se),
              sprintf("(%.2f)", panel_results[[decade_order[3]]]$se),
              sprintf("(%.2f)", panel_results[[decade_order[4]]]$se)))
  
  # Print intercept
  cat(sprintf("%-20s %12.2f %12.2f %12.2f %12.2f\n", "Intercept",
              panel_results[[decade_order[1]]]$intercept,
              panel_results[[decade_order[2]]]$intercept,
              panel_results[[decade_order[3]]]$intercept,
              panel_results[[decade_order[4]]]$intercept))
  
  # Print intercept SE
  cat(sprintf("%-20s %12s %12s %12s %12s\n", "",
              sprintf("(%.2f)", panel_results[[decade_order[1]]]$int_se),
              sprintf("(%.2f)", panel_results[[decade_order[2]]]$int_se),
              sprintf("(%.2f)", panel_results[[decade_order[3]]]$int_se),
              sprintf("(%.2f)", panel_results[[decade_order[4]]]$int_se)))
  
  # Print R-squared
  cat(sprintf("%-20s %12.2f %12.2f %12.2f %12.2f\n", "R-squared",
              panel_results[[decade_order[1]]]$r2,
              panel_results[[decade_order[2]]]$r2,
              panel_results[[decade_order[3]]]$r2,
              panel_results[[decade_order[4]]]$r2))
  
  # Print weighted mean
  cat(sprintf("%-20s %12.2f %12.2f %12.2f %12.2f\n", "Weighted mean",
              panel_results[[decade_order[1]]]$wmean,
              panel_results[[decade_order[2]]]$wmean,
              panel_results[[decade_order[3]]]$wmean,
              panel_results[[decade_order[4]]]$wmean))
  
  results[[panel_labels[p]]] <- panel_results
}

cat(sprintf("\nn = %d consistent industries\n", 
            n_distinct(reg_data$ind1990dd)))
cat("Weighted by mean industry share of total employment in FTEs\n")
cat("Computer use = annual pct point change 1984-1997\n")

# --- Save results object for later use ---
saveRDS(results, file.path(OUTPUT, "01_tables", "tableIII_results.rds"))
cat("\nSaved tableIII_results.rds\n")