#' Simulate coefficients from a GLM by making draws from the multivariate
#' normal distribution
#'
#' @param obj a fitted model object.
#' @param mu an optional vector giving the means of the variables. If \code{obj}
#' is supplied then \code{mu} is ignored.
#' @param Sigma an optional positive-definite symmetric matrix specifying the
#' covariance matrix of the variables. If \code{obj} is supplied then
#' \code{Sigma} is ignored.
#' @param nsim number of simulations to draw.
#'
#' @return A data frame of simulated coefficients from \code{obj}.
#'
#' @examples
#' library(car)
#'
#' # Estimate model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Create fitted values
#' prestige_sims <- b_sim(m1)
#'
#' # Manually supply coefficient means and covariance matrix
#' coefs <- coef(m1)
#' vcov_matrix <- vcov(m1)
#'
#' prestige_sims_manual <- b_sim(mu = coefs, Sigma = vcov_matrix)
#'
#' @importFrom stats coef vcov
#' @importFrom MASS mvrnorm
#'
#' @export

b_sim <- function(obj, mu, Sigma, nsim = 1000)
{
    if (!missing(obj)) {
        if (!missing(mu) || !missing(Sigma))
            message('obj used rather than mu and/or Sigma')
        obj_coef <- coef(obj)
        obj_vcov <- vcov(obj)
    }
    else if (missing(obj)) {
        obj_coef <- mu
        obj_vcov <- Sigma
    }

    drawn <- mvrnorm(n = nsim, mu = obj_coef, Sigma = obj_vcov)
    drawn <- data.frame(drawn)

    if (grepl('intercept', names(drawn[1]), ignore.case = TRUE))
        names(drawn)[1] <- 'intercept_'
    return(drawn)
}
