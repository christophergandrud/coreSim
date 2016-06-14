# Test b_sim -------------------------------------------------------------------
test_that('b_sim output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    # Survival model (no intercept term)
    library(survival)
    test1 <- list(time = c(4,3,1,1,2,2,3),
                  status = c(1,1,1,0,1,1,0),
                  x = c(0,2,1,1,1,0,0),
                  sex = c(0,0,0,0,1,1,1))
    # Fit a stratified model
    m_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
    m_coxph <- sim_coxph <- b_sim(m_coxph)


    expect_match(names(m1_sims)[[1]], 'intercept_')
    expect_match(names(sim_coxph)[[1]], 'x')
})

# Test linear_systematic -------------------------------------------------------
test_that('linear_systematic output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    fitted_df <- expand.grid(education = 6:16, typewc = 1)
    ls_lm <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)
    ls_lm_no_intercept <- linear_systematic(b_sims = m1_sims,
                                            newdata = fitted_df,
                                            inc_intercept = FALSE)

    # Survival model (no intercept term)
    library(survival)
    test1 <- list(time = c(4,3,1,1,2,2,3),
                  status = c(1,1,1,0,1,1,0),
                  x = c(0,2,1,1,1,0,0),
                  sex = c(0,0,0,0,1,1,1))
    # Fit a stratified model
    m_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
    m_coxph <- sim_coxph <- b_sim(m_coxph)
    ls_coxph <- linear_systematic(sim_coxph, newdata = data.frame(x = 1))

    expect_is(ls_lm$ls_, 'numeric')
    expect_true(mean(ls_lm$ls_) < mean(ls_lm_no_intercept$ls_))
    expect_is(ls_coxph$ls_, 'numeric')
})

# Test qi_builder --------------------------------------------------------------
test_that('qi_builder output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    fitted_df <- expand.grid(education = 6:16, typewc = 1)
    linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

    linear_qi_c1 <- qi_builder(b_sims = m1_sims, newdata = fitted_df, ci = 1)

    linear_qi_slim <- qi_builder(b_sims = m1_sims, newdata = fitted_df,
                                 slim = TRUE)

    # Predicted probabilities from logistic regression
    URL <- 'http://www.ats.ucla.edu/stat/data/binary.csv'
    Admission <- read.csv(URL)
    Admission$rank <- as.factor(Admission$rank)
    m2 <- glm(admit ~ gre + gpa + rank, data = Admission, family = 'binomial')
    m2_sims <- b_sim(m2)
    m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(2, 4),
                             rank4 = 1)
    pr_function <- function(x) 1 / (1 + exp(x))
    logistic_qi <- qi_builder(m2_sims, m2_fitted, model = pr_function)

    expect_is(linear_qi$qi_, 'numeric')
    expect_is(logistic_qi$qi_, 'numeric')
    expect_equal(nrow(linear_qi_c1), 11000)
    expect_equal(nrow(linear_qi_slim), 11)
    expect_equal(names(linear_qi_slim), c('education', 'typewc', 'qi_min',
                                       'qi_median', 'qi_max'))
})

# Test qi_slimmer --------------------------------------------------------------
test_that('qi_slimmer output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    fitted_df <- expand.grid(education = 6:16, typewc = 1)
    linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

    linear_slim <- qi_slimmer(linear_qi)

    expect_equal(nrow(linear_slim), 11)
    expect_equal(names(linear_slim), c('education', 'typewc', 'qi_min',
                                       'qi_median', 'qi_max'))
})
