# Example: Using fixest (feols) estimator with eventstudyr
# This example demonstrates the new feols estimator option

library(eventstudyr)
library(fixest)

# Basic feols estimation
results_feols <- EventStudy(
  estimator = "feols",
  data = example_data,
  outcomevar = "y_base",
  policyvar = "z",
  idvar = "id",
  timevar = "t",
  FE = TRUE,
  TFE = TRUE,
  post = 2,
  pre = 2,
  normalize = -1,
  cluster = TRUE
)

# View summary
summary(results_feols$output)

# Create event study plot
plt <- EventStudyPlot(results_feols)
print(plt)

# Test for pre-trends and leveling off
test_results <- TestLinear(results_feols)
print(test_results)

# Example with additional fixed effects
# (Requires data with additional categorical variables)
# results_feols_additional <- EventStudy(
#   estimator = "feols",
#   data = your_data,
#   outcomevar = "outcome",
#   policyvar = "treatment",
#   idvar = "unit_id",
#   timevar = "period",
#   additional_fixed_effects = "industry+region",
#   FE = TRUE,
#   TFE = TRUE,
#   post = 3,
#   pre = 2,
#   cluster = TRUE
# )

# Comparison with OLS (should give very similar results)
results_ols <- EventStudy(
  estimator = "OLS",
  data = example_data,
  outcomevar = "y_base",
  policyvar = "z",
  idvar = "id",
  timevar = "t",
  FE = TRUE,
  TFE = TRUE,
  post = 2,
  pre = 2,
  normalize = -1,
  cluster = TRUE
)

# Compare coefficients
coef_feols <- coef(results_feols$output)
coef_ols <- coef(results_ols$output)

eventstudy_vars <- results_feols$arguments$eventstudy_coefficients
comparison <- data.frame(
  variable = eventstudy_vars,
  feols = coef_feols[eventstudy_vars],
  ols = coef_ols[eventstudy_vars],
  difference = coef_feols[eventstudy_vars] - coef_ols[eventstudy_vars]
)

print("Coefficient Comparison (feols vs OLS):")
print(comparison)
print(paste("Maximum absolute difference:", max(abs(comparison$difference), na.rm = TRUE)))