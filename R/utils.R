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


#' Find interactions in fitted data frame
#'
#' @noRd

interaction_builder <- function(b_sims, newdata) {
    fitted_names <- names(newdata)
    sim_names <- names(b_sims)

    possible_inter <- max_period_count(sim_names)

    if (possible_inter != 0) {
        possible_inter <- possible_inter + 1
        if (length(fitted_names) < possible_inter)
            possible_inter <- length(fitted_names)

        for (k in 2:possible_inter) {
            interactions <- intersect(
                possible_interaction_terms(fitted_names, n = k), sim_names)

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

#' Find the maximum number of periods in the elements of a character string
#'
#' @noRd

max_period_count <- function(x) {
    max(nchar(x) - nchar(gsub('\\.', '', x)))
}

#' Find all possible interaction terms
#'
#' @noRd

possible_interaction_terms <- function(x, n = 2) {

    if (n > length(x))
        stop('Number of possible interaction terms cannot exceed number of base terms.')

    one_set <- function(base_names, x2) {
        if (missing(x2)) x2 <- base_names
        comb <- vector()
        for (i in base_names) {
            temp_vec <- x2[-grep(i, x2)]
            temp_comb1 <- paste(i, temp_vec, sep = '.')
            temp_comb2 <- paste(i, rev(temp_vec), sep = '.')
            comb <- c(comb, temp_comb1, temp_comb2)
            comb <- comb[!duplicated(comb)]
        }
        return(comb)
    }

    two_way <- one_set(x)
    temp_list <- list(two_way)

    if (n > 2) {
        for (u in 2:(n-1)) {
            if (u == 2) temp_list[[u]] <- one_set(x, unlist(temp_list[1]))
            else temp_list[[u]] <- one_set(x, unlist(temp_list[u-1]))
        }
    }

    out <- unlist(temp_list[[n-1]])

    return(out)
}

#' Convert \code{ci} interval from percent to proportion and check if valid
#'
#' @noRd

ci_check <- function(x) {
    if (x > 1 & x <= 100) x <- x / 100
    if (x <= 0 | x > 1) {
        stop(sprintf("%s is not a valid central interval.", x),
             call. = FALSE)
    }
    return(x)
}