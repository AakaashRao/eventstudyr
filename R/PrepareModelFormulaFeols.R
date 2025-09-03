#' Prepares model formula for feols estimation
#'
#' @param outcomevar Character indicating column of outcome variable.
#' @param str_policy_vars Character vector of policy variable names with leads/lags.
#' @param static Logical indicating if this is a static model.
#' @param controls Optional character vector of control variables.
#' @param additional_fixed_effects Optional character string of additional fixed effects.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param FE Logical for unit fixed effects.
#' @param TFE Logical for time fixed effects.
#'
#' @return A formula object for fixest::feols()
#' @importFrom stats as.formula
#' @keywords internal
#' @noRd

PrepareModelFormulaFeols <- function(outcomevar, str_policy_vars, static,
                                     controls = NULL, additional_fixed_effects = NULL,
                                     idvar = NULL, timevar = NULL, 
                                     FE = FALSE, TFE = FALSE) {
    
    # Build the main regression terms
    regressors <- c(str_policy_vars, controls)
    
    # Build the fixed effects specification
    if (FE | TFE | !is.null(additional_fixed_effects)) {
        fes <- c()
        
        if (FE) {
            fes <- c(fes, idvar)
        }
        if (TFE) {
            fes <- c(fes, timevar)
        }
        
        # Parse and add additional fixed effects
        if (!is.null(additional_fixed_effects)) {
            # Split by '+' to handle multiple fixed effects
            additional_fes <- trimws(strsplit(additional_fixed_effects, "\\+")[[1]])
            fes <- c(fes, additional_fes)
        }
        
        # Construct formula with fixed effects using fixest syntax
        formula_str <- paste(
            outcomevar,
            "~",
            paste(regressors, collapse = " + "),
            "|",
            paste(fes, collapse = " + ")
        )
        formula <- stats::as.formula(formula_str)
    } else {
        # No fixed effects - standard formula
        formula <- stats::reformulate(
            termlabels = regressors,
            response = outcomevar,
            intercept = TRUE
        )
    }
    
    return(formula)
}