#' Find quantities of interest from generalized linear models
#'
#' @param obj a fitted model object from which to base coefficient simulations
#' on.
#' @param newdata an optional data frame of fitted values with column names
#' corresponding to coefficient names in \code{obj} or \code{mu}/\code{Sigma}.
#' Note that variables not included in \code{newdata} will be fitted at 0. If
#' \code{missing} then observations used to fit the model in \code{obj} will be
#' used.
#' @param FUN a function for calculating how to find the quantity of interest
#' from a vector of the fitted linear systematic component. It must return
#' a numeric vector. If \code{missing} then a normal
#' linear regression model is assumed and the predicted values are returned
#' (i.e. the fitted linear systematic component from
#' \code{\link{linear_systematic}}).
#' @param nsim number of simulations to draw.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in (0, 1] or equivalently (0, 100]. Note: if \code{ci = 1}
#' then the full interval (i.e. 100 percent) is assumed.
#' @param slim logical indicating whether to (if \code{FALSE}) return all
#' simulations in the central interval specified by \code{ci} for each fitted
#' scenario or (if \code{TRUE}) just the minimum, median, and maxium values.
#' See \code{\link{qi_slimmer}} for more details.
#' @param large_computation logical. If \code{newdata} is not supplied,
#' whether to allow > 100000 simulated quantities of interest to be found.
#' @param original_order logical whether or not to keep the original scenario
#' order when \code{slim = TRUE}. Choosing \code{FALSE} can imporove computation
#' time.
#' @param b_sims an optional data frame created by \code{\link{b_sim}} of
#' simulated coefficients. Only used if \code{obj} is not supplied.
#' @param mu an optional vector giving the means of the variables. If \code{obj}
#' or \code{b_sims} is supplied then \code{mu} is ignored.
#' @param Sigma an optional positive-definite symmetric matrix specifying the
#' covariance matrix of the variables. If \code{obj} is supplied then
#' \code{Sigma} is ignored. If your model includes an intercept, this should be
#' given the name \code{intercept_}.
#' @param verbose logical. Whether to include full set of messages or not.
#' @param ... arguments to passed to \code{\link{linear_systematic}}.
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
#' # Manually supply coefficient means and covariance matrix
#' coefs <- coef(m1)
#' vcov_matrix <- vcov(m1)
#'
#' linear_qi_custom_mu_Sigma <- qi_builder(mu = coefs, Sigma = vcov_matrix,
#'                                  newdata = fitted_df_1)
#'
#' ## Logistic regression
#' # Load data
#' data(Admission)
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
#' @importFrom dplyr bind_rows
#'
#' @export

qi_builder <- function(obj, newdata, FUN, ci = 0.95, nsim = 1000,
                      slim = FALSE,  large_computation = FALSE,
                      original_order = FALSE,
                      b_sims, mu, Sigma, verbose = TRUE,
                      ...)
{
    qi_ <- NULL

    ci <- ci_check(ci)

    if (!missing(obj)) b_sims <- b_sim(obj = obj, nsim = nsim)

    else if (missing(obj) & !missing(b_sims)) b_sims <- b_sims

    else if (missing(obj) & missing(b_sims) & !missing(mu) & !missing(Sigma))
        b_sims <- b_sim(mu = mu, Sigma = Sigma, nsim = nsim)

    if (missing(newdata) & !missing(obj))
        newdata <- find_scenarios(obj = obj, nsim = nsim,
                                 large_computation = large_computation)
    if (missing(newdata) & missing(obj))
        stop('At least one of obj and newdata must be supplied to find simulation scenarios.')

    qi_df <- linear_systematic(b_sims = b_sims, newdata = newdata, ...)

    if (missing(FUN)) {
        if (verbose)
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
        if (original_order) {
            orig_order <- unique(qi_df[, 1:(ncol(qi_df)-1)])
            orig_order <- apply(orig_order, 1, paste, collapse = '.')
        }

        qi_df$scenario_ <- interaction(qi_df[, 1:(ncol(qi_df)-1)])
        if (original_order)
            qi_df$scenario_ <- factor(qi_df$scenario_, levels = orig_order)

        qi_df <- qi_central_interval(qi_df, scenario_var = 'scenario_',
                                     qi_var= 'qi_', ci = ci)

        if (!isTRUE(slim)) qi_df$scenario_ <- NULL
    }

    if (slim) qi_df <- qi_slimmer(qi_df)

    return(qi_df)
}
