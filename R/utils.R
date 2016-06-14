#' Find maximum, minimum, and median values for each scenario found using
#' \code{\link{qi_builder}}
#'
#' @param df a data frame of simulated quantities of interest created by
#' \code{\link{qi_builder}}.
#'
#' @return A data frame with the fitted values and the minimum (\code{qi_min}),
#' median (\code{qi_median}), and maximum (\code{qi_max}) values from the
#' central interval specified with \code{ci} in \code{\link{qi_builder}}.
#'
#' @details This funciton slims down a simulation data set to some of its key
#' features (minimun, median, and maximum value for each fitted scenario) so that
#' it takes up less memory and can be easily plotted.
#'
#' The function is incorporated into \code{\link{qi_builder}} and can be run
#' using \code{slim = TRUE}.
#'
#' @examples
#' library(car)
#'
#' # Normal linear model
#' m1 <- lm(prestige ~ education + type, data = Prestige)
#'
#' # Simulate coefficients
#' m1_sims <- b_sim(m1)
#'
#' # Create fitted values
#' fitted_df <- expand.grid(education = 6:16, typewc = 1)
#'
#' # Find predicted outcomes (95% central interval, by default)
#' linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)
#'
#' # Slim data set
#' linear_slim <- qi_slimmer(linear_qi)
#'
#' @importFrom stats median
#' @importFrom dplyr group_by summarise %>%
#'
#' @export

qi_slimmer <- function(df){
    qi_ <- scenario_ <- NULL

    if (!(names(df)[[ncol(df)]] == 'scenario_'))
        df$scenario_ <- interaction(df[, 1:(ncol(df)-1)], drop = TRUE)

    df_out <- df %>% group_by(scenario_) %>%
        summarise(qi_min = min(qi_),
                  qi_median = median(qi_),
                  qi_max = max(qi_)
                  ) %>%
        data.frame

    scenarios_df <- df[!duplicated(df$scenario_), 1:(ncol(df)-2)] %>%
        data.frame(row.names = NULL)
    df_out <- cbind(scenarios_df, df_out)
    df_out$scenario_ <- NULL
    return(df_out)
}


#' Find two way interactions in fitted data frame
#'
#' @noRd

interaction_builder <- function(b_sims, newdata) {
    fitted_names <- names(newdata)
    sim_names <- names(b_sims)
    interactions <- intersect(
        combn(fitted_names, 2, FUN = paste, collapse = '.'), sim_names)

    if (length(interactions) != 0) {
        for (i in interactions) {
            if (length(intersect(i, fitted_names)) == 0) {
                sub_df <- data.frame()
                for (u in fitted_names) {
                    if (grepl(u, i))
                        sub_df <- cbind_fill(sub_df, u = newdata[[u]])
                }
                newdata[, i] <- apply(sub_df, 1, prod)
            }
        }
    }
    return(newdata)
}


#' Bind column to empty data.frame
#'
#' @source http://stackoverflow.com/a/26685092/1705044
#'
#' @noRd

cbind_fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
