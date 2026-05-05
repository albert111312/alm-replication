# ==============================================================================
# 01c_build_crosswalks.R
# Build and save ind1990dd crosswalk
# ==============================================================================

ind1990dd_crosswalk <- tibble(
  ind1990 = c(12, 142, 220, 232, 290, 321, 350, 380, 381, 
                422, 442, 510, 590, 592, 602, 632, 640, 660, 
                750, 790, 830, 851, 863, 900, 941:960),
  ind1990dd = c(30, 150, 222, 241, 301, 322, 342, 391, 391,
                  432, 441, 532, 600, 600, 611, 682, 682, 682,
                  742, 791, 840, 860, 862, 901, rep(940, length(941:960)))
  )
saveRDS(ind1990dd_crosswalk, file.path(DATA_INT, "ind1990dd_crosswalk.rds"))
cat("Created ind1990dd_crosswalk.rds\n")


# ==============================================================================
# NOTE: This script is a direct translation of David Dorn's C4 file 
# (subfile_ind1990dd.do located in the 06_crosswalks folder). The .do file
# contains a series of replace ind1990dd = X if ind1990 == Y commands that
# recode specific ind1990 values into broader groups. I manually extracted those 
# recoding rules and hardcoded them as an R tibble with 44 rows mapping ind1990
# values to their ind1990dd targets. The script is functionally identical to
# running Dorn's .do file in Stata. It has the same input codes, same output
# codes, and same logic.
# ==============================================================================

