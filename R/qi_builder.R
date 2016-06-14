#' Find quantities of interest from generalized linear models
#'
#' @param b_sims a data frame created by \code{\link{b_sim}} of simulated
#' coefficients
#' @param newdata a data frame of fitted values with column names corresponding
#' to variable names in \code{b_sims}. If \code{missing} then a normal
#' linear regression model is assumed and the predicted values are returned
#' (e.g. the fitted linear systematic component from
#' \code{\link{linear_systematic}}).
#' @param model a function for calculating how to find the quantity of interest
#' from a vector of the fitted linear systematic component.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in (0, 1].
#' @param slim logical indicating whether to (if \code{FALSE}) return all
#' simulations in the central interval specified by \code{ci} for each fitted
#' scenario or (if \code{TRUE}) just the minimum, median, and maxium values.
#' See \code{\link{qi_slimmer}} for more details.
#' @param ... arguments to pass to \code{\link{linear_systematic}}.
#'
#' @return If \code{slimmer = FALSE} data frame of fitted values supplied in
#' \code{newdata} and associated simulated quantities of interest for all
#' simulations in the central interval specified by \code{ci}. The quantities
#' of interest are in a column named \code{qi_}.
#'
#' @examples
#' library(car)
#'
#' ## Normal linear model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#' # Simulate coefficients
#' m1_sims <- b_sim(m1)
#'
#' # Create fitted values
#' fitted_df_1 <- expand.grid(education = 6:16, typewc = 1)
#'
#' linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df_1)
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
#' # Simulate coefficients
#' m2_sims <- b_sim(m2)
#'
#' # Create fitted values
#' m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(2, 4),
#'                          rank4 = 1)
#'
#' # Function to find predicted probabilities from logistic regression models
#' pr_function <- function(x) 1 / (1 + exp(x))
#'
#' # Find quantity of interest
#' logistic_qi <- qi_builder(m2_sims, m2_fitted, model = pr_function)
#'
#' logistic_qi <- qi_builder(m2_sims, m2_fitted, model = pr_function,
#'                          slim = TRUE)
#'
#' @importFrom stats quantile
#' @importFrom dplyr bind_rows
#'
#' @export

qi_builder <- function(b_sims, newdata, model, ci = 0.95, slim = FALSE, ...) {
    qi_ <- NULL
    if (ci <= 0 | ci > 1) {
        stop("ci must be greater than 0 and not greater than 1.",
             call. = FALSE)
    }

    qi_df <- linear_systematic(b_sims = b_sims, newdata = newdata, ...)

    if (missing(model)) {
        message('Note: model argument missing -> assuming normal linear model.\n')
        names(qi_df)[grep('ls_', names(qi_df))] <- 'qi_'
    }
    else {
        if (!is.function(model)) stop('model must be a function.',
            call. = FALSE)
        qi_df[, 'qi_'] <- model(qi_df[['ls_']])
        qi_df['ls_'] <- NULL
    }

    if (ci < 1) {
        lower <- (1 - ci)/2
        upper <- 1 - lower

        qi_df$scenario_ <- interaction(qi_df[, 1:(ncol(qi_df)-1)])
        qi_list <- split(qi_df, qi_df[['scenario_']])

        qi_list <- lapply(1:length(qi_list), function(x){
            lower_bound <- quantile(qi_list[[x]][,'qi_'], prob = lower)
            upper_bound <- quantile(qi_list[[x]][,'qi_'], prob = upper)
            subset(qi_list[[x]], qi_ >= lower_bound & qi_ <= upper_bound)
        })

        qi_df <- data.frame(bind_rows(qi_list))
        qi_df$scenario_ <- NULL
    }

    if (slim) qi_df <- qi_slimmer(qi_df)

    return(qi_df)
}
