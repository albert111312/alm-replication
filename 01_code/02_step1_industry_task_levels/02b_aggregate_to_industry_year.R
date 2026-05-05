# ==============================================================================
# 02b_aggregate_to_industry_year.R
# Build industry-gender-education cells for all years using consistent industries
# ==============================================================================

worker_tasks <- readRDS(file.path(DATA_INT, "worker_tasks.rds"))

# --- Define consistent industry set ---
# Industries present in every year-source combination in the worker data.
# The join with computer use data in 06a_merge_final_dataset.R will further
# restrict to industries with valid computer use observations.
year_source_df <- bind_rows(
  ALM_DECADES %>% select(year = year0, source = source0),
  ALM_DECADES %>% select(year = year1, source = source1)
) %>% distinct()

consistent_inds <- worker_tasks %>%
  filter(ind1990dd > 0) %>%
  pull(ind1990dd) %>%
  unique()

for (i in seq_len(nrow(year_source_df))) {
  inds <- worker_tasks %>%
    filter(year == year_source_df$year[i], source == year_source_df$source[i], ind1990dd > 0) %>%
    pull(ind1990dd) %>% unique()
  consistent_inds <- intersect(consistent_inds, inds)
}

cat("Consistent industries:", length(consistent_inds), "\n")

# --- Build industry-gender-education cells for all years ---
task_vars <- unname(unlist(DOT_TASK_VARS))

all_cells <- worker_tasks %>%
  filter(ind1990dd %in% consistent_inds, fte_wt > 0) %>%
  group_by(year, source, ind1990dd, sex, educ_group) %>%
  summarise(
    across(all_of(task_vars), ~ weighted.mean(.x, fte_wt, na.rm = TRUE)),
    cell_fte = sum(fte_wt),
    .groups = "drop"
  )

cat("Total cells across all years:", nrow(all_cells), "\n")
all_cells %>% count(year, source) %>% print()

saveRDS(all_cells, file.path(DATA_INT, "industry_cells.rds"))
cat("Saved industry_cells.rds\n")
