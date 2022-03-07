# Example code for the cSEM package that accompanies:
   
# Venturini, S. and Mehmetoglu, M. (2022) Software Packages for Partial Least
# Squares Structural Equation Modeling: An Updated Review, in
# Latan, H. and Noonan, R., Partial Least Squares Path Modeling, 2nd edition,
# Springer, 2022

if (!require(cSEM, quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
  install_github("M-E-Rademaker/cSEM")
  library(cSEM)
}

path_data <- ""    # place here the path where to find the data
corprep <- read.csv(file.path(path_data, "Corporate Reputation Data.csv"))
corprep[which(corprep == -99, arr.ind = TRUE)] <- NA  # replace -99 with NA

corp.mod <- "
  # measurement model
  LIKE =~ like_1 + like_2 + like_3
  COMP =~ comp_1 + comp_2 + comp_3
  CUSA =~ cusa
  CUSL =~ cusl_1 + cusl_2 + cusl_3
  CSOR <~ csor_1 + csor_2 + csor_3 + csor_4 + csor_5
  ATTR <~ attr_1 + attr_2 + attr_3
  PERF <~ perf_1 + perf_2 + perf_3 + perf_4 + perf_5
  QUAL <~ qual_1 + qual_2 + qual_3 + qual_4 + qual_5 + qual_6 + qual_7 + qual_8
  
  # structural model
  LIKE ~ QUAL + CSOR + PERF + ATTR
  COMP ~ QUAL + CSOR + PERF + ATTR
  CUSA ~ COMP + LIKE
  CUSL ~ COMP + LIKE + CUSA
"
corp.indic <- c(
  "like_1", "like_2", "like_3",
  "comp_1", "comp_2", "comp_3",
  "cusa",
  "cusl_1", "cusl_2", "cusl_3",
  "csor_1", "csor_2", "csor_3", "csor_4", "csor_5",
  "attr_1", "attr_2", "attr_3",
  "perf_1", "perf_2", "perf_3", "perf_4", "perf_5",
  "qual_1", "qual_2", "qual_3", "qual_4", "qual_5", "qual_6", "qual_7", "qual_8")

corprep.est <- csem(
  .data = na.omit(corprep[, corp.indic]),  # na.omit() is used to remove missing
  .model = corp.mod,
  .PLS_weight_scheme_inner = "factorial",
  .disattenuate = FALSE,                   # this argument disables PLSc
  .tolerance = 1e-07,
  .iter_max = 100,
  .PLS_modes =
    list(LIKE = "modeA", COMP = "modeA",
         CUSA = "modeA", CUSL = "modeA",
         CSOR = "modeB", ATTR = "modeB",
         PERF = "modeB", QUAL = "modeB"),
  .resample_method = "bootstrap",
  .R = 500,
  .seed = 123)

summarize(.object = corprep.est, .ci = "CI_percentile", .alpha = 0.05)
assess(corprep.est)
# exportToExcel(
  # .postestimation_object = assess(corprep.est), 
  # .filename = "results.xlsx")

# Multigroup analysis (not reported in the chapter)
corprep.mga <- csem(
  .data = na.omit(corprep[, c(corp.indic, "education")]),
  .model = corp.mod,
  .PLS_weight_scheme_inner = "factorial",
  .disattenuate = FALSE,
  .tolerance = 1e-07,
  .iter_max = 100,
  .PLS_modes =
    list(LIKE = "modeA", COMP = "modeA",
         CUSA = "modeA", CUSL = "modeA",
         CSOR = "modeB", ATTR = "modeB",
         PERF = "modeB", QUAL = "modeB"),
  .resample_method = "bootstrap",
  .R = 500,
  .seed = 123,
  .id = "education")

summarize(corprep.mga, .ci = "CI_percentile", .alpha = 0.05)
assess(corprep.mga)

corprep.mgd <- testMGD(
  .object = corprep.mga,
  .R_bootstrap = 500,
  .R_permutation = 500,
  .seed = 123)
print(.object = corprep.mgd, .approach_mgd = "Chin")

# Importance-performance map analysis (not reported in the chapter)
corprep.ipma <- doIPMA(.object = corprep.est)
plot(
  x = corprep.ipma,
  .dependent = "CUSL",
  .level = "construct",
  .attributes = c("QUAL", "CSOR", "PERF", "ATTR", "COMP", "LIKE", "CUSA"))
plot(
  x = corprep.ipma,
  .dependent = "CUSL",
  .level = "indicator",
  .attributes = corp.indic)

# Nonlinear effects analysis (not reported in the chapter)
corp.int <- "
  # measurement model
  LIKE =~ like_1 + like_2 + like_3
  COMP =~ comp_1 + comp_2 + comp_3
  CUSA =~ cusa
  CUSL =~ cusl_1 + cusl_2 + cusl_3
  CSOR <~ csor_1 + csor_2 + csor_3 + csor_4 + csor_5
  ATTR <~ attr_1 + attr_2 + attr_3
  PERF <~ perf_1 + perf_2 + perf_3 + perf_4 + perf_5
  QUAL <~ qual_1 + qual_2 + qual_3 + qual_4 + qual_5 + qual_6 + qual_7 + qual_8
  
  # structural model
  LIKE ~ QUAL + CSOR + PERF + ATTR
  COMP ~ QUAL + CSOR + PERF + ATTR
  CUSA ~ COMP + LIKE
  CUSL ~ COMP + LIKE + CUSA + COMP.LIKE + COMP.CUSA + LIKE.CUSA
"
corprep.int <- csem(
  .data = na.omit(corprep[, corp.indic]),  # na.omit() is used to remove missing
  .model = corp.int,
  .PLS_weight_scheme_inner = "factorial",
  .disattenuate = FALSE,                   # this argument disables PLSc
  .tolerance = 1e-07,
  .iter_max = 100,
  .PLS_modes =
    list(LIKE = "modeA", COMP = "modeA",
         CUSA = "modeA", CUSL = "modeA",
         CSOR = "modeB", ATTR = "modeB",
         PERF = "modeB", QUAL = "modeB"))

neffects <- doNonlinearEffectsAnalysis(corprep.int,
                                       .dependent = 'CUSL',
                                       .moderator = 'CUSA',
                                       .independent = 'COMP')
neffects
plot(neffects, .plot_type = 'simpleeffects')
plot(neffects, .plot_type = 'floodlight')

# Measurement model invariance (not reported in the chapter)
testMICOM(corprep.mga, .R = 500, .seed = 123)
