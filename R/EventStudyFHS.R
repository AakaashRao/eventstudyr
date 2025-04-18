#' Implements proxy Instrumental Variables (IV) estimator proposed in [Freyaldenhoven Hansen and Shapiro (FHS, 2019)](https://www.aeaweb.org/articles?id=10.1257/aer.20180609)
#'
#' @param prepared_model_formula A formula object created in PrepareModelForumla that is passed to EventStudy.
#' @param prepared_data Data frame containing all of the parameters required for [EventStudy()] plus leads and
#' lags of the first differenced policy variable and leads and lags of the policy variable.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE. Must be TRUE if FE is TRUE.
#'
#' @return A data.frame that contains the estimates for the event study coefficients.
#' @import estimatr
#' @importFrom stats qnorm pnorm
#' @keywords internal
#' @noRd
#'
#' @examples
#' model_formula <-  PrepareModelFormula(
#'    estimator = "FHS",
#'    outcomevar = "y_base",
#'    str_policy_fd = c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2"),
#'    str_policy_lead = "z_lead3",
#'    str_policy_lag = "z_lag3",
#'    controls = "x_r",
#'    proxy = "eta_m",
#'    proxyIV = "z_fd_lead3"
#')
#'
#' EventStudyFHS(
#'    prepared_model_formula = model_formula,
#'    prepared_data = df_EventStudyFHS_example,
#'    idvar = "id",
#'    timevar = "t",
#'    FE = TRUE,
#'    TFE = TRUE,
#'    cluster = TRUE
#')

EventStudyFHS <- function(prepared_model_formula, prepared_data,
                          idvar, timevar, FE, TFE, cluster) {

  if (! inherits(prepared_model_formula, "formula")) {stop("prepared_model_formula should be a formula")}
  if (! is.data.frame(prepared_data)) {stop("data should be a data frame.")}
  if (! is.character(idvar)) {stop("idvar should be a character.")}
  if (! is.character(timevar)) {stop("timevar should be a character.")}
  if (! is.logical(FE)) {stop("FE should be either TRUE or FALSE.")}
  if (! is.logical(TFE)) {stop("TFE should be either TRUE or FALSE.")}
  if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}
  if (FE & !cluster) {stop("cluster=TRUE required when FE=TRUE.")}


  if (FE & TFE & cluster) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          clusters = get(idvar),
          fixed_effects = ~ get(idvar) + get(timevar),
          se_type="stata")

      N <- fhs_output$nobs
      n <- fhs_output$nclusters
      K <- length(fhs_output$felevels$`get(timevar)`) + fhs_output$rank

      fhs_output$std.error <- fhs_output$std.error / sqrt((N - K) / (N - n - K + 1))
      fhs_output$statistic <- fhs_output$coefficients / fhs_output$std.error
      fhs_output$p.value <- 2*stats::pnorm(abs(fhs_output$statistic), lower.tail = FALSE)
      fhs_output$conf.low <- fhs_output$coefficients - stats::qnorm(0.975) * fhs_output$std.error
      fhs_output$conf.high <- fhs_output$coefficients + stats::qnorm(0.975) * fhs_output$std.error
      fhs_output$vcov <- fhs_output$vcov / ((N - K) / (N - n - K + 1))

  } else if (FE & (!TFE) & cluster) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          clusters = get(idvar),
          fixed_effects = ~ get(idvar),
          se_type="stata")

      N <- fhs_output$nobs
      n <- fhs_output$nclusters
      K <- 1 + fhs_output$rank

      fhs_output$std.error <- fhs_output$std.error / sqrt((N - K)/(N - n - K + 1))
      fhs_output$statistic <- fhs_output$coefficients / fhs_output$std.error
      fhs_output$p.value <- 2*stats::pnorm(abs(fhs_output$statistic), lower.tail = FALSE)
      fhs_output$conf.low <- fhs_output$coefficients - stats::qnorm(0.975) * fhs_output$std.error
      fhs_output$conf.high <- fhs_output$coefficients + stats::qnorm(0.975) * fhs_output$std.error
      fhs_output$vcov <- fhs_output$vcov / ((N - K) / (N - n - K + 1))

  } else if ((!FE) & TFE & (!cluster)) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          fixed_effects = ~ get(timevar),
          se_type="stata")

  } else if ((!FE) & TFE & cluster) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          clusters = get(idvar),
          fixed_effects = ~ get(timevar),
          se_type="stata")

  } else if ((!FE) & (!TFE) & (!cluster)) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          se_type = "stata")

  } else if ((!FE) & (!TFE) & cluster) {

      fhs_output <- estimatr::iv_robust(
          formula = prepared_model_formula,
          data = prepared_data,
          clusters = get(idvar),
          se_type = "stata")
  }

  return(fhs_output)
}

