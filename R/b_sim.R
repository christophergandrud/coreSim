#' Simulate coefficients from a GLM by making draws from the multivariate
#' normal distribution
#'
#' @param obj a fitted model object.
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
#' @importFrom stats coef vcov
#' @importFrom MASS mvrnorm
#'
#' @export

b_sim <- function(obj, nsim = 1000)
{
    obj_coef <- coef(obj)
    obj_vcov <- vcov(obj)

    drawn <- mvrnorm(n = nsim, mu = obj_coef, Sigma = obj_vcov)
    drawn <- data.frame(drawn)

    if (grepl('intercept', names(drawn[1]), ignore.case = TRUE))
        names(drawn)[1] <- 'intercept_'
    return(drawn)
}
