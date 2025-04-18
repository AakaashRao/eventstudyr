#' Creates an Event-Study Plot Following the Suggestions in Freyaldenhoven et al. (2021)
#'
#' @description `EventStudyPlot` takes the output from [EventStudy()] and combines it with additional optional arguments to facilitate constructing an Event-Study Plot.
#'
#' @param estimates The output from calling [EventStudy()]. Should be a list of length 2.
#' @param xtitle The title for the x-axis. Should be a string. Defaults to "Event time".
#' @param ytitle The title for the y-axis. Should be a string. Defaults to "Coefficient".
#' @param ybreaks A vector containing the desired breaks for the y-axis.
#' Defaults to NULL, which means the breaks are computed automatically.
#' If custom breaks are selected with the `add_mean` argument set to TRUE, then the breaks must include zero.
#' @param conf_level Confidence level used for confidence interval
#' expressed as a real number between 0 and 1, inclusive. Defaults to 0.95.
#' @param supt The confidence level used for obtaining the sup-t bands critical value.
#' Should be a real number between 0 and 1, inclusive. Defaults to .95. Sup-t bands are simulation-based,
#' so you must set a seed if you would like your sup-t band results to be reproducible (see examples).
#' @param num_sim The number of simulations used in generating the sup-t bands.
#' Should be a natural number. Defaults to 1000.
#' @param add_mean Adds the mean of the dependent variable in the period used for normalization.
#' Should be TRUE or FALSE. Defaults to FALSE.
#' @param pre_event_coeffs If TRUE, uses pre and overidpre from estimates to test for pre-trends.
#' Should be TRUE or FALSE. Defaults to TRUE.
#' @param post_event_coeffs If TRUE, uses post and overidpost from estimates to test for leveling-off.
#' Should be TRUE or FALSE. Defaults to TRUE.
#' @param add_zero_line Whether or not to plot a dashed horizontal line at y = 0.
#' Should be TRUE or FALSE. Defaults to TRUE, meaning the line is plotted.
#' @param smpath Plot smoothest path of confounder that rationalizes event study coefficients.
#' Should be TRUE or FALSE. Defaults to FALSE.
#'
#' @return The Event-Study plot as a ggplot2 object.
#' @import ggplot2 dplyr
#' @import estimatr
#' @importFrom rlang .data
#' @importFrom data.table setorder
#' @export
#'
#' @examples
#'
#' #
#'
#' # Minimal examples
#' ### OLS
#'
#' estimates_ols <- EventStudy(
#'    estimator = "OLS",
#'    data = example_data,
#'    outcomevar = "y_smooth_m",
#'    policyvar = "z",
#'    idvar = "id",
#'    timevar = "t",
#'    controls = "x_r",
#'    FE = TRUE, TFE = TRUE,
#'    post = 3, overidpost = 5,
#'    pre = 2,  overidpre = 4,
#'    normalize = - 3
#' )
#'
#' plt_ols <- EventStudyPlot(estimates = estimates_ols)
#' plt_ols
#'
#' ### IV
#'
#' estimates_fhs <- EventStudy(
#'    estimator = "FHS",
#'    data = example_data,
#'    outcomevar = "y_smooth_m",
#'    policyvar = "z",
#'    idvar = "id",
#'    timevar = "t",
#'    proxy = "x_r",
#'    post = 2, overidpost = 1,
#'    pre = 0,  overidpre = 3,
#'    normalize = -1
#' )
#'
#' plt_fhs <- EventStudyPlot(estimates = estimates_fhs)
#' plt_fhs
#'
#' # Optional arguments
#'
#' ### Change x- and y-axis titles and set ybreaks
#' EventStudyPlot(estimates = estimates_ols,
#'                xtitle = "Relative time", ytitle = "",
#'                ybreaks = seq(-2, 1, 0.5))
#'
#' ### Add smoothest path
#' EventStudyPlot(estimates = estimates_ols, smpath = TRUE)
#'
#' ### Add y-mean to y-axis and line y = 0
#' EventStudyPlot(estimates = estimates_ols, add_mean = TRUE,
#'                add_zero_line = TRUE)
#'
#' ### Do not plot supt bands
#' EventStudyPlot(estimates = estimates_ols, supt = NULL)
#'
#' ### Setting seed prior to plotting sup-t bands
#' set.seed(1234)
#' EventStudyPlot(estimates = estimates_ols)
#'
#' # Modify plots using ggplot2 functions
#' library(ggplot2)
#'
#' ### Change color of dots, horizontal line, and theme
#' plt_ols +
#'   geom_point(color = "red") +
#'   geom_hline(color = "gray", yintercept = 0) +
#'   theme_light() +
#'   theme(panel.grid.minor.x = element_blank())
#'

EventStudyPlot <- function(estimates,
                           xtitle = "Event time", ytitle = "Coefficient", ybreaks = NULL,
                           conf_level = .95, supt = .95, num_sim = 1000, add_mean = FALSE,
                           pre_event_coeffs = TRUE, post_event_coeffs = TRUE,
                           add_zero_line = TRUE, smpath = FALSE) {

    if (!is.character(xtitle))    {stop("Argument 'xtitle' should be a character.")}
    if (!is.character(ytitle))    {stop("Argument 'ytitle' should be a character.")}
    if (!is.logical(add_zero_line)) {stop("Argument 'add_zero_line' should be either TRUE or FALSE.")}
    if (!is.null(ybreaks) &
        !is.numeric(ybreaks))     {stop("Argument 'ybreaks' should be NULL or a numeric vector.")}

# Estimation Elements -----------------------------------------------------

    df_estimates      <- estimates$output
    df_estimates_tidy <- estimatr::tidy(estimates$output)

    static_model <- nrow(df_estimates_tidy) == 1
    if (static_model) {
        stop("EventStudyPlot() does not support static models.")
    }

    df_data                 <- estimates$arguments$data
    outcomevar              <- estimates$arguments$outcomevar
    policyvar               <- estimates$arguments$policyvar
    post                    <- estimates$arguments$post
    overidpost              <- estimates$arguments$overidpost
    pre                     <- estimates$arguments$pre
    overidpre               <- estimates$arguments$overidpre
    normalize               <- estimates$arguments$normalize
    normalization_column    <- estimates$arguments$normalization_column
    eventstudy_coefficients <- estimates$arguments$eventstudy_coefficients
    proxyIV                 <- estimates$arguments$proxyIV

# Optionally Add Suptbands/Confidence Intervals ---------------------------

    plot_supt <- if(!is.null(supt)) TRUE else FALSE

    if (plot_supt) {
        df_estimates_tidy <- AddSuptBand(df_estimates, num_sim = 1000, conf_level = supt,
                                         eventstudy_coefficients = eventstudy_coefficients)
    }

    plot_CI <- if(!is.null(conf_level)) TRUE else FALSE

    if (plot_CI) {

        df_estimates_tidy <- AddCIs(df_estimates_tidy, eventstudy_coefficients, conf_level)
    }

# Optionally Test For Pretrends/Levelling-Off -----------------------------

    df_test_linear <- TestLinear(estimates = estimates, pretrends = pre_event_coeffs, leveling_off = post_event_coeffs)

    if ((pre_event_coeffs | post_event_coeffs)) {
        pretrends_p_value   <- df_test_linear[df_test_linear["Test"] == "Pre-Trends",   "p.value"]
        levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]

        text_pretrends   <- paste0("Pretrends p-value = ", round(pretrends_p_value, 2))
        text_levelingoff <- paste0("Leveling off p-value = ", round(levelingoff_p_value, 2))


        if (pre_event_coeffs & post_event_coeffs) {
            text_caption <- paste0(text_pretrends, " -- ", text_levelingoff)

        } else if (pre_event_coeffs & !post_event_coeffs) {
            text_caption <- text_pretrends

        } else if (!pre_event_coeffs & post_event_coeffs) {
            text_caption <- text_levelingoff

        }
    } else {
        text_caption <- NULL
    }


    df_plt <- PreparePlottingData(df_estimates_tidy, policyvar,
                                  post, overidpost, pre, overidpre, normalization_column, proxyIV)

# Construct y breaks ------------------------------------------------------

    if (!is.null(ybreaks)) {
        if (!(0 %in% ybreaks) & add_mean) {
            stop("If you want to add the mean of y in the y-axis then 'ybreaks' must include 0.")
        }

        ylabels <- ybreaks
        ylims   <- c(min(ybreaks), max(ybreaks))
    } else {
        min_value <- min(c(df_plt$estimate, df_plt$ci_lower, df_plt$suptband_lower), na.rm = T)
        max_value <- max(c(df_plt$estimate, df_plt$ci_upper, df_plt$suptband_upper), na.rm = T)
        max_abs   <- max(abs(min_value), abs(max_value))

        magnitude <- 10^floor(log10(max_abs))

        # Determine step depending on how far the endpoints are from the magnitude
        mean_ratio <- mean(c(abs(min_value)/magnitude, max_value/magnitude))
        if (mean_ratio > 6.67) {
            step = 3*magnitude
        } else if (mean_ratio > 3.33) {
            step = 2*magnitude
        } else {
            step = magnitude
        }

        # Pick multiples of step to ensure zero is included
        close_to_min <- floor(min_value/step)*step
        close_to_max <- ceiling(max_value/step)*step

        ybreaks <- seq(close_to_min, close_to_max, step)
        ylims   <- c(min(ybreaks), max(ybreaks))

        if (length(ybreaks) >= 9) {
            # Too many breaks, double step size
            step         <- step*2
            close_to_min <- floor(min_value/step)*step
            close_to_max <- ceiling(max_value/step)*step

            ybreaks <- seq(close_to_min, close_to_max, step)
        } else if (length(ybreaks) <= 3) {
            # Too few breaks, halve step size
            step         <- step/2
            close_to_min <- floor(min_value/step)*step
            close_to_max <- ceiling(max_value/step)*step

            ybreaks <- seq(close_to_min, close_to_max, step)
        }
        ylabels <- ybreaks
    }

# Optionally Adds Mean ----------------------------------------------------

    if (add_mean) {

        y_mean <- AddMeans(df_data, normalization_column, policyvar, outcomevar)

        index_zero <- which(ybreaks == 0)
        ylabels[index_zero] <- paste0(ylabels[index_zero], " (", round(y_mean, 2), ")")
    }

# Optionally Add smooth path ----------------------------------------------

    # Order coefficients
    label_var = "label"
    data.table::setorderv(df_plt, c(label_var))
    ordered_labels <- df_plt$label

    if (smpath) {

        unselect_message <- "Please change the 'Smpath' argument in 'EventStudyPlot' to FALSE."

        if (!is.null(proxyIV)) {
            if (sum(df_plt$estimate == 0) > 2) {
                stop(paste0("The smoothest path is not supported for the FHS estimator with more than one instrument.",
                            unselect_message))
            }
        }

        coefficients <- df_plt$estimate

        # Add column and row in matrix of coefficients in index of norm columns
        covar <- AddZerosCovar(estimates$output$vcov,
                               eventstudy_coefficients,
                               df_plt[df_plt$estimate==0, ]$term,
                               df_plt$term)

        inv_covar <- pracma::pinv(covar)

        df_plt <- AddSmPath(df_plt, coefficients, inv_covar)
    }

# Construct Plot ----------------------------------------------------------

    df_plt$label_num <- as.numeric(gsub("+", "", df_plt$label, fixed = T))

    plt <- ggplot(df_plt,
                  aes(x = .data$label_num, y = .data$estimate))

    if (add_zero_line) {
        plt <- plt +
            geom_hline(yintercept = 0,
                       color = "green", linetype = "dashed")
    }
    if (plot_supt) {
        plt <- plt +
            geom_linerange(aes(ymin = .data$suptband_lower,
                               ymax = .data$suptband_upper),
                               data = df_plt[df_plt$estimate != 0,])
    }
    if (plot_CI) {
        plt <- plt +
            geom_errorbar(aes(ymin = .data$ci_lower,
                              ymax = .data$ci_upper),
                              data = df_plt[df_plt$estimate != 0,],
                              width = .2)
    }
    if (smpath) {
        plt <- plt +
            geom_line(aes(y = .data$smoothest_path, group = 1),
                      color = "black")
    }

    plt <- plt +
        geom_point(color = "#006600") +
        scale_x_continuous(breaks = min(df_plt$label_num):max(df_plt$label_num),
                           labels = ordered_labels) +
        scale_y_continuous(breaks = ybreaks,
                           labels = ylabels,
                           limits = ylims) +
        labs(x = xtitle, y = ytitle,
             caption = text_caption) +
        theme_bw() +
        theme(panel.grid   = element_blank(),
              plot.caption = element_text(hjust = 0))

    return(plt)
}
