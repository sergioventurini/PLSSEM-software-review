# Example code for the SEMinR package that accompanies:
   
# Venturini, S. and Mehmetoglu, M. (2022) Software Packages for Partial Least
# Squares Structural Equation Modeling: An Updated Review, in
# Latan, H. and Noonan, R., Partial Least Squares Path Modeling, 2nd edition,
# Springer, 2022

if (!require(seminr, quietly = TRUE)) {
  install.packages("seminr")
  library(seminr)
}

data("corp_rep_data", package = "seminr")

# Specification of the measurement model
corprep_mm <- constructs(
  composite("PERF", multi_items("perf_", 1:5), weights = mode_B),
  composite("CSOR", multi_items("csor_", 1:5), weights = mode_B),
  composite("ATTR", multi_items("attr_", 1:3), weights = mode_B),
  composite("QUAL", multi_items("qual_", 1:8), weights = mode_B),
  composite("COMP", multi_items("comp_", 1:3), weights = mode_A),
  composite("LIKE", multi_items("like_", 1:3), weights = mode_A),
  composite("CUSA", single_item("cusa"), weights = mode_A),
  composite("CUSL", multi_items("cusl_", 1:3), weights = mode_A))

# Specification of the structural model
corprep_sm <- relationships(
  paths(from = c("PERF", "CSOR", "ATTR", "QUAL"), to = c("COMP", "LIKE")),
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

# Model estimation
corp_indic <- c(
  "like_1", "like_2", "like_3",
  "comp_1", "comp_2", "comp_3",
  "cusa",
  "cusl_1", "cusl_2", "cusl_3",
  "csor_1", "csor_2", "csor_3", "csor_4", "csor_5",
  "attr_1", "attr_2", "attr_3",
  "perf_1", "perf_2", "perf_3", "perf_4", "perf_5",
  "qual_1", "qual_2", "qual_3", "qual_4", "qual_5", "qual_6", "qual_7", "qual_8")

casewise_deletion <- function(data) {
  return(data)  # this function does nothing!
}

corprep_est <- estimate_pls(
  data = corp_rep_data[, corp_indic],
  measurement_model = corprep_mm,
  structural_model = corprep_sm,
  missing_value = -99,
  inner_weights = path_factorial,
  missing = casewise_deletion)

plot(corprep_est, title = "Corporate Reputation Model")

corprep_sum <- summary(corprep_est)
corprep_sum
# str(corprep_sum)
plot(corprep_sum$reliability)

nc <- parallel::detectCores()
corprep_boot <- bootstrap_model(corprep_est, nboot = 500, cores = nc, seed = 123)

corprep_boot_sum <- summary(corprep_boot, alpha = 0.05)
corprep_boot_sum
plot(corprep_boot, title = "Corporate Reputation Model (using bootstrap)")

# Multigroup analysis (not reported in the chapter)
corprep_est <- estimate_pls(
  data = corp_rep_data[, c("education", corp_indic)],
  measurement_model = corprep_mm,
  structural_model = corprep_sm,
  missing_value = -99,
  inner_weights = path_factorial,
  missing = casewise_deletion)

## education == 1 vs. all the rest
corprep_mga_1 <- estimate_pls_mga(
  pls_model = corprep_est,
  condition = corp_rep_data$education == 1,
  nboot = 500,
  seed = 123)
corprep_mga_1

## education == 2 vs. all the rest
corprep_mga_2 <- estimate_pls_mga(
  pls_model = corprep_est,
  condition = corp_rep_data$education == 2,
  nboot = 500,
  seed = 123)
corprep_mga_2

## education == 3 vs. all the rest
corprep_mga_3 <- estimate_pls_mga(
  pls_model = corprep_est,
  condition = corp_rep_data$education == 3,
  nboot = 500,
  seed = 123)
corprep_mga_3

## education == 4 vs. all the rest
corprep_mga_4 <- estimate_pls_mga(
  pls_model = corprep_est,
  condition = corp_rep_data$education == 4,
  nboot = 500,
  seed = 123)
corprep_mga_4
