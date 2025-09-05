#' Runs fixed effects OLS using fixest::feols
#'
#' @param prepared_model_formula A formula object created in PrepareModelFormulaFeols() that is passed to EventStudy().
#' @param prepared_data Data frame containing all of the parameters required for EventStudy() plus leads and
#' lags of the first differenced policy variable and leads and lags of the policy variable.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE.
#'
#' @return A fixest object containing the estimates for the event study coefficients.
#' @import fixest
#' @keywords internal
#' @noRd

EventStudyFeols <- function(prepared_model_formula, prepared_data,
                           idvar, timevar, FE, TFE, cluster) {
    
    if (! inherits(prepared_model_formula, "formula")) {stop("prepared_model_formula should be a formula")}
    if (! is.data.frame(prepared_data)) {stop("data should be a data frame.")}
    if (! is.character(idvar)) {stop("idvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.logical(FE)) {stop("FE should be either TRUE or FALSE.")}
    if (! is.logical(TFE)) {stop("TFE should be either TRUE or FALSE.")}
    if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}
    
    # Run feols estimation with explicit clustering
    if (cluster) {
        # Use formula notation for clustering to match estimatr behavior
        cluster_formula <- as.formula(paste0("~", idvar))
        feols_output <- fixest::feols(
            fml = prepared_model_formula,
            data = prepared_data,
            cluster = cluster_formula,
            ssc = fixest::ssc(adj = TRUE, fixef.K = "nested"),
            lean = FALSE
        )
    } else {
        feols_output <- fixest::feols(
            fml = prepared_model_formula,
            data = prepared_data,
            se = "hetero",
            lean = FALSE
        )
    }
    return(feols_output)
}