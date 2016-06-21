#' Find quantities of interest from generalized linear models
#'
#' @param obj a fitted model object from which to base coefficient simulations
#' on.
#' @param newdata an optional data frame of fitted values with column names
#' corresponding to variable names in \code{b_sims}. Note that variabless not
#' included in \code{newdata} will be fitted at 0. If \code{missing}
#' then observations used to fit the model in \code{obj} will be used.
#' @param FUN a function for calculating how to find the quantity of interest
#' from a vector of the fitted linear systematic component. It must return
#' a numeric vector. If \code{missing} then a normal
#' linear regression model is assumed and the predicted values are returned
#' (i.e. the fitted linear systematic component from
#' \code{\link{linear_systematic}}).
#' @param nsim number of simulations to draw.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in (0, 1] or equivalently (0, 100].
#' @param slim logical indicating whether to (if \code{FALSE}) return all
#' simulations in the central interval specified by \code{ci} for each fitted
#' scenario or (if \code{TRUE}) just the minimum, median, and maxium values.
#' See \code{\link{qi_slimmer}} for more details.
#' @param b_sims an optional data frame created by \code{\link{b_sim}} of
#' simulated coefficients. Only used if \code{obj} is not supplied.
#' @param large_computation logical. If \code{newdata} is not supplied,
#' whether to allow > 100000 simulated quantities of interest to be found.
#' @param original_order logical whether or not to keep the original scenario
#' order when \code{slim = TRUE}. Choosing \code{FALSE} can imporove computation time.
#' @param ... arguments to pass to
#' \code{\link{linear_systematic}}.
#'
#' @return If \code{slimmer = FALSE} a data frame of fitted values supplied in
#' \code{newdata} and associated simulated quantities of interest for all
#' simulations in the central interval specified by \code{ci}. The quantities
#' of interest are in a column named \code{qi_}.
#'
#' If \code{slimmer = TRUE} a data frame of fitted values supplied in
#' \code{newdata} and the minimum, median, and maximum values of the central
#' interval specified by \code{ci} for each scenario are returned in three
#' columns named \code{qi_min}, \code{qi_median}, and \code{qi_max},
#' respectively.
#'
#' @examples
#' library(car)
#'
#' ## Normal linear model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Using observed data as scenarios
#' linear_qi_obs <- qi_builder(m1)
#'
#' # Create fitted values
#' fitted_df_1 <- expand.grid(education = 6:16, typewc = 1)
#'
#' linear_qi <- qi_builder(m1, newdata = fitted_df_1)
#'
#' ## Logistic regression
#' # Download data
#' URL <- 'http://www.ats.ucla.edu/stat/data/binary.csv'
#' Admission <- read.csv(URL)
#' Admission$rank <- as.factor(Admission$rank)
#'
#' # Estimate model
#' m2 <- glm(admit ~ gre + gpa + rank, data = Admission, family = 'binomial')
#'
#' # Specify fitted values
#' m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(2, 4),
#'                          rank = '4')
#'
#' # Function to find predicted probabilities from logistic regression models
#' pr_function <- function(x) 1 / (1 + exp(-x))
#'
#' # Find quantity of interest
#' logistic_qi_1 <- qi_builder(m2, m2_fitted, FUN = pr_function)
#'
#' logistic_qi_2 <- qi_builder(m2, m2_fitted, FUN = pr_function,
#'                          slim = TRUE)
#'
#' @importFrom stats quantile
#' @importFrom dplyr bind_rows
#'
#' @export

qi_builder <- function(obj, newdata, FUN, ci = 0.95, nsim = 1000,
                      slim = FALSE, b_sims, large_computation = FALSE,
                      original_order = FALSE,
                      ...)
{
    qi_ <- NULL
    ci <- ci_check(ci)

    if (!missing(obj)) b_sims <- b_sim(obj = obj, nsim = nsim)

    if (missing(newdata) & !missing(obj))
        newdata <- find_scenarios(obj = obj, nsim = nsim,
                                 large_computation = large_computation)
    if (missing(newdata) & missing(obj))
        stop('At least one of obj and newdata must be supplied to find simulation scenarios.')

    qi_df <- linear_systematic(b_sims = b_sims, newdata = newdata, ...)

    if (missing(FUN)) {
        message('Note: FUN argument missing -> assuming b_sims is from a normal linear model.\n')
        names(qi_df)[grep('ls_', names(qi_df))] <- 'qi_'
    }
    else {
        FUN_check(FUN)

        temp_qi <- FUN(qi_df[['ls_']])

        FUN_results_check(temp_qi)

        qi_df[, 'qi_'] <- temp_qi
        qi_df['ls_'] <- NULL
    }

    if (ci < 1) {
        lower <- (1 - ci)/2
        upper <- 1 - lower

        if (original_order) {
            orig_order <- unique(qi_df[, 1:(ncol(qi_df)-1)])
            orig_order <- apply(orig_order, 1, paste, collapse = '.')
        }

        qi_df$scenario_ <- interaction(qi_df[, 1:(ncol(qi_df)-1)])
        if (original_order)
            qi_df$scenario_ <- factor(qi_df$scenario_, levels = orig_order)
        qi_list <- split(qi_df, qi_df[['scenario_']])

        qi_list <- lapply(1:length(qi_list), function(x){
            lower_bound <- quantile(qi_list[[x]][,'qi_'], prob = lower)
            upper_bound <- quantile(qi_list[[x]][,'qi_'], prob = upper)
            subset(qi_list[[x]], qi_ >= lower_bound & qi_ <= upper_bound)
        })

        qi_df <- data.frame(bind_rows(qi_list))
        if (!isTRUE(slim)) qi_df$scenario_ <- NULL
    }

    if (slim) qi_df <- qi_slimmer(qi_df)

    return(qi_df)
}
