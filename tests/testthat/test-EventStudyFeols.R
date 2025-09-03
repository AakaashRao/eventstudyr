# Test suite for feols estimator in EventStudy

test_that("feols estimator produces near-identical results to OLS", {
    
    # Run OLS estimation
    results_ols <- suppressWarnings(
        EventStudy(
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
    )
    
    # Run feols estimation with same specification
    results_feols <- suppressWarnings(
        EventStudy(
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
    )
    
    # Extract coefficients from both models
    coef_ols <- coef(results_ols$output)
    coef_feols <- coef(results_feols$output)
    
    # Get event study coefficient names (excluding fixed effects)
    eventstudy_coefs <- results_ols$arguments$eventstudy_coefficients
    
    # Compare coefficients for event study variables
    for (var in eventstudy_coefs) {
        if (var %in% names(coef_ols) & var %in% names(coef_feols)) {
            expect_equal(coef_feols[var], coef_ols[var], tolerance = 1e-6,
                        label = paste("Coefficient for", var))
        }
    }
    
    # Also compare standard errors and t-statistics
    # Note: fixest and estimatr use slightly different clustered SE calculations
    # Differences of ~1.7% are expected and acceptable
    tidy_ols <- estimatr::tidy(results_ols$output)
    tidy_feols <- broom::tidy(results_feols$output)
    
    for (var in eventstudy_coefs) {
        ols_row <- tidy_ols[tidy_ols$term == var,]
        feols_row <- tidy_feols[tidy_feols$term == var,]
        
        if (nrow(ols_row) > 0 & nrow(feols_row) > 0) {
            # Standard errors should be very close (within 3% given different implementations)
            # Use as.numeric to strip attributes from fixest output
            expect_equal(as.numeric(feols_row$std.error), as.numeric(ols_row$std.error), 
                        tolerance = 0.03,
                        label = paste("Std error for", var))
            
            # T-statistics should also be very close (within 3%)
            expect_equal(as.numeric(feols_row$statistic), as.numeric(ols_row$statistic), 
                        tolerance = 0.03,
                        label = paste("T-statistic for", var))
        }
    }
})

test_that("additional_fixed_effects parameter works correctly", {
    
    # This test would need actual data with additional variables
    # For now, we test that the parameter is accepted
    expect_error(
        EventStudy(
            estimator = "feols",
            data = example_data,
            outcomevar = "y_base",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            additional_fixed_effects = "t",  # Using time as additional FE for testing
            FE = TRUE,
            TFE = FALSE,
            post = 1,
            pre = 1,
            cluster = TRUE
        ),
        NA  # Expect no error
    )
})

test_that("additional_fixed_effects only works with feols estimator", {
    
    # Should error with OLS
    expect_error(
        EventStudy(
            estimator = "OLS",
            data = example_data,
            outcomevar = "y_base",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            additional_fixed_effects = "some_var",
            FE = TRUE,
            TFE = TRUE,
            post = 1,
            pre = 1,
            cluster = TRUE
        ),
        "additional_fixed_effects should only be specified when estimator = 'feols'"
    )
    
    # Should error with FHS
    expect_error(
        EventStudy(
            estimator = "FHS",
            data = example_data,
            outcomevar = "y_base",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            proxy = "x_r",
            additional_fixed_effects = "some_var",
            FE = TRUE,
            TFE = TRUE,
            post = 1,
            pre = 1,
            cluster = TRUE
        ),
        "additional_fixed_effects should only be specified when estimator = 'feols'"
    )
})

test_that("feols works with static models", {
    
    results_feols_static <- suppressWarnings(
        EventStudy(
            estimator = "feols",
            data = example_data,
            outcomevar = "y_jump_m",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            FE = TRUE,
            TFE = TRUE,
            post = 0,
            overidpost = 0,
            pre = 0,
            overidpre = 0,
            cluster = TRUE
        )
    )
    
    expect_s3_class(results_feols_static$output, "fixest")
    expect_true(length(coef(results_feols_static$output)) >= 1)
})

test_that("feols works without fixed effects", {
    
    results_feols_no_fe <- suppressWarnings(
        EventStudy(
            estimator = "feols",
            data = example_data,
            outcomevar = "y_base",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            FE = FALSE,
            TFE = FALSE,
            post = 1,
            pre = 1,
            cluster = FALSE
        )
    )
    
    expect_s3_class(results_feols_no_fe$output, "fixest")
})

test_that("feols works with controls", {
    
    results_feols_controls <- suppressWarnings(
        EventStudy(
            estimator = "feols",
            data = example_data,
            outcomevar = "y_base",
            policyvar = "z",
            idvar = "id",
            timevar = "t",
            controls = "x_r",
            FE = TRUE,
            TFE = TRUE,
            post = 2,
            pre = 1,
            cluster = TRUE
        )
    )
    
    expect_s3_class(results_feols_controls$output, "fixest")
    # Check that control variable coefficient exists
    expect_true("x_r" %in% names(coef(results_feols_controls$output)))
})