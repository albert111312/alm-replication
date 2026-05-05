# ==============================================================================
# generate_figures.R
# Create coefficient plots and task trend figures
# ==============================================================================

# ==============================================================================
# FIGURE 1: Weighted Mean Task Changes by Decade
# ==============================================================================

# Part I weighted means
wmean_rows <- list()
for (dv in dep_vars) {
  for (dec in decade_order) {
    d <- reg_data %>% filter(decade == dec)
    wmean_rows <- c(wmean_rows, list(tibble(
      panel = panel_labels[dv],
      decade = dec,
      period = "Part I (DOT)",
      wmean = weighted.mean(d[[dv]], d$emp_weight)
    )))
  }
}

# Extension weighted means
for (dv in dep_vars) {
  for (dec in ext_decade_order) {
    d <- ext_reg %>% filter(decade == dec)
    if (nrow(d) == 0) next
    wmean_rows <- c(wmean_rows, list(tibble(
      panel = panel_labels[dv],
      decade = dec,
      period = "Part II (O*NET)",
      wmean = weighted.mean(d[[dv]], d$emp_weight)
    )))
  }
}

all_wmeans <- bind_rows(wmean_rows) %>%
  mutate(
    decade = factor(decade, levels = c("1960-1970", "1970-1980", "1980-1990",
                                       "1990-1998", "2000-2010", "2010-2020")),
    panel = factor(panel, levels = c("A. Nonroutine analytic",
                                     "B. Nonroutine interactive",
                                     "C. Routine cognitive",
                                     "D. Routine manual"))
  )

fig1 <- ggplot(all_wmeans, aes(x = decade, y = wmean, fill = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_col(width = 0.6) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = c("Part I (DOT)" = "#4393c3", "Part II (O*NET)" = "#7fbc41")) +
  labs(
    title = "Figure 1: Economy-Wide Task Changes by Decade",
    subtitle = "Weighted mean of 10 × annual within-industry change in task percentiles",
    x = NULL,
    y = "Weighted mean task change\n(centiles per decade)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(OUTPUT, "02_figures", "fig1_task_trends.png"),
       fig1, width = 10, height = 8, dpi = 300)
cat("Saved fig1_task_trends.png\n")


# ==============================================================================
# FIGURE 2: Coefficient Plot — Replication vs ALM
# ==============================================================================

reg_data <- readRDS(file.path(DATA_FINAL, "tableIII_data.rds"))

dep_vars <- c("d_GED_MATH", "d_DCP", "d_STS", "d_FINGDEX")
panel_labels <- c(
  "d_GED_MATH" = "A. Nonroutine analytic",
  "d_DCP"      = "B. Nonroutine interactive",
  "d_STS"      = "C. Routine cognitive",
  "d_FINGDEX"  = "D. Routine manual"
)
decade_order <- c("1960-1970", "1970-1980", "1980-1990", "1990-1998")

# Build replication coefficients
repl_rows <- list()
for (dv in dep_vars) {
  for (dec in decade_order) {
    d <- reg_data %>% filter(decade == dec)
    mod <- lm(as.formula(paste(dv, "~ delta_computer")),
              data = d, weights = emp_weight)
    s <- summary(mod)
    repl_rows <- c(repl_rows, list(tibble(
      panel = panel_labels[dv],
      decade = dec,
      source = "Replication",
      coef = coef(mod)[2],
      se = s$coefficients[2, 2]
    )))
  }
}

# ALM published coefficients
alm_rows <- tribble(
  ~panel, ~decade, ~source, ~coef, ~se,
  "A. Nonroutine analytic",    "1960-1970", "ALM (2003)",  7.49,  5.28,
  "A. Nonroutine analytic",    "1970-1980", "ALM (2003)",  9.11,  4.17,
  "A. Nonroutine analytic",    "1980-1990", "ALM (2003)", 14.02,  4.97,
  "A. Nonroutine analytic",    "1990-1998", "ALM (2003)", 12.04,  4.74,
  "B. Nonroutine interactive", "1960-1970", "ALM (2003)",  7.55,  6.64,
  "B. Nonroutine interactive", "1970-1980", "ALM (2003)", 10.81,  5.71,
  "B. Nonroutine interactive", "1980-1990", "ALM (2003)", 17.21,  6.32,
  "B. Nonroutine interactive", "1990-1998", "ALM (2003)", 14.78,  5.48,
  "C. Routine cognitive",      "1960-1970", "ALM (2003)",  3.90,  4.48,
  "C. Routine cognitive",      "1970-1980", "ALM (2003)",-11.00,  5.40,
  "C. Routine cognitive",      "1980-1990", "ALM (2003)",-13.94,  5.72,
  "C. Routine cognitive",      "1990-1998", "ALM (2003)",-17.57,  5.54,
  "D. Routine manual",         "1960-1970", "ALM (2003)",  4.15,  3.50,
  "D. Routine manual",         "1970-1980", "ALM (2003)", -6.56,  4.84,
  "D. Routine manual",         "1980-1990", "ALM (2003)", -5.94,  5.64,
  "D. Routine manual",         "1990-1998", "ALM (2003)",-24.72,  5.77
)

# Extension coefficients
ext_reg <- readRDS(file.path(DATA_FINAL, "tableIII_extension_data.rds"))
ext_decade_order <- c("2000-2010", "2010-2020")

ext_rows <- list()
for (dv in dep_vars) {
  for (dec in ext_decade_order) {
    d <- ext_reg %>% filter(decade == dec)
    if (nrow(d) == 0) next
    mod <- lm(as.formula(paste(dv, "~ delta_computer")),
              data = d, weights = emp_weight)
    s <- summary(mod)
    ext_rows <- c(ext_rows, list(tibble(
      panel = panel_labels[dv],
      decade = dec,
      source = "Extension",
      coef = coef(mod)[2],
      se = s$coefficients[2, 2]
    )))
  }
}

# Combine all
all_coefs <- bind_rows(bind_rows(repl_rows), alm_rows, bind_rows(ext_rows)) %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    decade = factor(decade, levels = c("1960-1970", "1970-1980", "1980-1990",
                                       "1990-1998", "2000-2010", "2010-2020")),
    panel = factor(panel, levels = c("A. Nonroutine analytic",
                                     "B. Nonroutine interactive",
                                     "C. Routine cognitive",
                                     "D. Routine manual")),
    source = factor(source, levels = c("ALM (2003)", "Replication", "Extension"))
  )

fig2 <- ggplot(all_coefs, aes(x = decade, y = coef, color = source, shape = source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi),
                  position = position_dodge(width = 0.5), size = 0.5) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_color_manual(values = c("ALM (2003)" = "#2166ac",
                                "Replication" = "#b2182b",
                                "Extension" = "#4dac26")) +
  scale_shape_manual(values = c("ALM (2003)" = 17, "Replication" = 16, "Extension" = 15)) +
  labs(
    title = "Figure 2: Coefficient Estimates by Decade",
    subtitle = "Coefficient on Δ Computer use with 95% confidence intervals",
    x = NULL,
    y = "Coefficient",
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(OUTPUT, "02_figures", "fig2_coefficient_plot.png"),
       fig2, width = 10, height = 8, dpi = 300)
cat("Saved fig2_coefficient_plot.png\n")
