context('qi_builder')

test_that('qi_builder output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    fitted_df <- expand.grid(education = c(7, 6, 8:16), typewc = 1)

    linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)
    linear_qi_c1 <- qi_builder(m1, newdata = fitted_df, ci = 1)
    linear_qi_slim1 <- qi_builder(m1, newdata = fitted_df, slim = TRUE)
    linear_qi_slim2 <- qi_builder(m1, newdata = fitted_df,
                                  original_order = TRUE, slim = TRUE)

    linear_qi_auto_newdata <- qi_builder(m1)

    # Manually supply coefficient means and covariance matrix
    coefs <- coef(m1)
    vcov_matrix <- vcov(m1)

    linear_qi_custom_mu_Sigma <- qi_builder(mu = coefs, Sigma = vcov_matrix,
                                            newdata = fitted_df)

    # Predicted probabilities from logistic regression
    URL <- 'http://www.ats.ucla.edu/stat/data/binary.csv'
    Admission <- read.csv(URL)
    Admission$rank <- as.factor(Admission$rank)
    m2 <- glm(admit ~ gre + gpa + rank, data = Admission, family = 'binomial')

    m2_sims <- b_sim(m2)

    m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(2, 4),
                             rank4 = 1)
    pr_function <- function(x) 1 / (1 + exp(-x))
    logistic_qi <- qi_builder(m2, m2_fitted, FUN = pr_function)

    expect_is(linear_qi$qi_, 'numeric')
    expect_is(logistic_qi$qi_, 'numeric')
    expect_equal(nrow(linear_qi_c1), 11000)
    expect_equal(nrow(linear_qi_slim1), 11)
    expect_equal(names(linear_qi_slim1), c('education', 'typewc', 'qi_min',
                                       'qi_median', 'qi_max'))
    expect_equal(nrow(linear_qi_auto_newdata), 88350)
    expect_false(all(linear_qi_slim1$education == linear_qi_slim2$education))
    expect_equal(nrow(linear_qi_custom_mu_Sigma), nrow(linear_qi))
    expect_error(qi_builder(m1, nsim = 20000))
    expect_error(qi_builder(m2, m2_fitted, FUN = pr_function, ci = 950))
    expect_error(qi_builder(m2, m2_fitted, FUN = function(x, y){x + y}))
    expect_error(qi_builder(m2, m2_fitted, FUN = function(x){x <- 'a'}))
    expect_error(qi_builder(m2, m2_fitted, FUN = function(x){x <- data.frame()}))
    expect_error(qi_builder(m2, m2_fitted, FUN = 'test_fail'))

    # Unsupported model for automatic newdata creation
    library(survival)
    test1 <- list(time = c(4,3,1,1,2,2,3),
                  status = c(1,1,1,0,1,1,0),
                  x = c(0,2,1,1,1,0,0),
                  sex = c(0,0,0,0,1,1,1))
    # Fit a stratified model
    m_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
    expect_error(qi_builder(m_coxph))

})
