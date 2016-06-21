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
#' linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df, slim = FALSE)
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
#' @source http://stackoverflow.com/a/26685092/1705044
#' @noRd

cbind_fill <- function(...){
    nm <- list(...)
    nm <- lapply(nm, as.matrix)
    n <- max(sapply(nm, nrow))
    do.call(cbind, lapply(nm, function (x)
        rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

#' Find the maximum number of periods in the elements of a character string
#' @noRd

max_period_count <- function(x) {
    x <- x[!(grepl('^I\\..*\\.[1-9].*$', x))]
    max(nchar(x) - nchar(gsub('\\.', '', x)))
}

#' Find all possible interaction terms
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
#' @noRd

ci_check <- function(x) {
    if (x > 1 & x <= 100) x <- x / 100
    if (x <= 0 | x > 1) {
        stop(sprintf("%s is not a valid central interval.", x),
             call. = FALSE)
    }
    return(x)
}

#' Check the user supplied QI function for consistency
#' @noRd

FUN_check <- function(x) {
    if (!is.function(x))
        stop('FUN must be a function.', call. = FALSE)
    if (length(formals(x)) != 1)
        stop('FUN can only have one argument.\nThis argument must accept only a numeric vector of the fitted linear component calculated from the simulations.',
             call. = FALSE)
}

#' Check results from user supplied function
#' @noRd

FUN_results_check <- function(x) {
    if (!is.vector(x))
        stop('FUN did not return a vector.', call. = FALSE)
    if (!is.numeric(x))
        stop('FUN returned a non-numeric vector.', call. = FALSE)
}

#' Find scenarios to simulate from fitted model objects
#' @noRd

find_scenarios <- function(obj, nsim, large_computation = FALSE) {
    # Supported model types for scenario extraction
    supported <- c('lm', 'glm')
    if (!any(supported %in% class(obj)))
        stop(sprintf('Scenarios cannot currently be extracted from %s models.\n Please define scenarios with newdata.',
            class(obj)), call. = FALSE)

    out <- obj$model[, -1]

    out <- out[!duplicated(out[, 1:ncol(out)]), ]

    if (!isTRUE(large_computation)) {
        unique_scenarios <- nrow(out)
        nout_sims <- nsim * unique_scenarios
            if (nout_sims > 100000)
                stop(sprintf('%s unique scenarios were found.\nWith %s simulations, %s simulated scenarios would be created.\n\nFor manageable computation, please supply a smaller number of scenarios to newdata.',
                             unique_scenarios, nsim, nout_sims), call. = FALSE)
    }
    return(out)
}

#' Convert factor levels into binary categorical values
#' @noRd

factorise <- function(x, b_sims) {
    x <- data.frame(x)
    comb <- x
    for (i in names(x)) {
        if(is.character(x[[i]]) | is.factor(x[[i]])) {
            unique_levels <- unique(x[[i]])
            new_cols <- sprintf('%s%s', i, unique_levels)

            cat_df <- data.frame(
                        matrix(0, nrow = nrow(x), ncol = length(new_cols)))
            names(cat_df) <- new_cols

            for (u in new_cols) cat_df[, u][x[[i]] == gsub(i, '', u)] <- 1
            comb <- comb[, !(names(comb) %in% i), drop = FALSE]
            comb <- cbind(comb, cat_df)
        }
    }
    comb <- comb[, names(comb) %in% names(b_sims), drop = FALSE]
    return(comb)
}

#' Find non-linear transformations of variables in newdata
#' @noRd

non_linear_transformer <- function(x, b_sims) {
    sim_names <- names(b_sims)

    # Polynomials
    ## Note: assumes poly <= 9
    if (any(grepl('^I\\..*\\.[1-9]\\.$', sim_names))) {
        sub_names <- sim_names[grepl('^I\\..*\\.[1-9].*$', sim_names)]
        sub_names <- sub_names[!(sub_names %in% names(x))]
        for (i in sub_names) {
            var_i <- gsub('^I\\.', '', i)
            var_i <- gsub('\\.[1-9]\\.$', '', var_i)

            if((var_i %in% colnames(x))) {
                pow_i <- as.numeric(substr(i, nchar(i)-1, nchar(i)-1))
                x[, i] <- x[, var_i]^pow_i
            }
        }
    }

    # Natural logarithms
    if (any(grepl('^log\\..*\\.$', sim_names))) {
        sub_names <- sim_names[grepl('^log\\..*\\.$', sim_names)]
        sub_names <- sub_names[!(sub_names %in% names(x))]
        for (i in sub_names) {
            var_i <- gsub('^log\\.', '', i)
            var_i <- gsub('\\.$', '', var_i)

            if((var_i %in% colnames(x))) {
                x[, i] <- log(x[, var_i])
            }
        }
    }

    return(x)
}
