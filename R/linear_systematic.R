#' Find the systematic component in the linear form for fitted values in across
#' each simulation (note largely for internal use in \code{\link{qi_builder}})
#'
#' @param b_sims a data frame created by \code{\link{b_sim}} of simulated
#' coefficients
#' @param newdata a data frame of fitted values with column names corresponding
#' to variable names in \code{b_sims}. Variables in \code{b_sim} not present
#' in \code{newdata} will be treated as fitted at 0. Interactions will
#' automatically be found if they were entered into to the model using the
#' \code{*} operator.
#' @param inc_intercept logical whether to include the intercept in the
#' lineary systematic component.
#'
#' @return A data frame fitted values supplied in \code{newdata} and associated
#' linear systematic component estimates for all simulationed coefficient
#' estimates. The linear systematic components are included in a column
#' named \code{ls_}.
#'
#' @examples
#' library(car)
#'
#' # Estimate model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Create fitted values
#' fitted_df <- expand.grid(education = 6:16, typewc = 1)
#'
#' # Simulate coefficients
#' m1_sims <- b_sim(m1, nsim = 1000)
#'
#' # Find linear systematic component for fitted values
#' ls <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)
#'
#' @source King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the
#' Most of Statistical Analyses: Improving Interpretation and Presentation."
#' American Journal of Political Science 44(2): 341-55.
#'
#' @importFrom dplyr bind_rows
#'
#' @export

linear_systematic <- function(b_sims, newdata, inc_intercept = TRUE)
{
    newdata <- non_linear_transformer(x = newdata, b_sims = b_sims)
    newdata <- factorise(x = newdata, b_sims = b_sims)

    fitted_names <- names(newdata)

    if (!('intercept_' %in% names(b_sims))) inc_intercept <- FALSE

    if (!all(fitted_names %in% names(b_sims)))
        stop('Unable to find all of the variables from newdata in b_sims.',
            call. = FALSE)

    if (!all(sapply(newdata, class) %in% c('numeric', 'integer')))
        stop('All fitted values must be either numeric or integer.',
            call. = FALSE)

    newdata <- interaction_builder(b_sims = b_sims, newdata = newdata)
    fitted_names <- names(newdata)

    intercept <- b_sims[['intercept_']]
    not_fitted_0 <- data.matrix(b_sims[, fitted_names])
    sims_fitted <- list(not_fitted_0, data.matrix(newdata))

    if (inc_intercept) {
        ls <- lapply(1:nrow(sims_fitted[[2]]), function(x) {
            fitted_1 <- sims_fitted[[2]][x, ]
            data.frame(
                data.frame(
                    t(fitted_1)),
                    ls_ = intercept + (sims_fitted[[1]] %*% fitted_1)
            )
        })
    }
    else {
        ls <- lapply(1:nrow(sims_fitted[[2]]), function(x) {
            fitted_1 <- sims_fitted[[2]][x, ]
            data.frame(
                data.frame(
                    t(fitted_1)),
                    ls_ = (sims_fitted[[1]] %*% fitted_1)
            )
        })
    }

    ls <- data.frame(bind_rows(ls))
    return(ls)
}
