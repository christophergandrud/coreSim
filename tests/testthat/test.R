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

    mpoly <- lm(prestige ~ education + I(education ^2) + type, data = Prestige)
    mpoly_sims <- b_sim(mpoly)
    ls_mpoly <- linear_systematic(b_sims = mpoly_sims, newdata = fitted_df)

    mlog <- lm(prestige ~ log(education) + type, data = Prestige)
    mlog_sims <- b_sim(mlog)
    ls_mlog <- linear_systematic(b_sims = mlog_sims, newdata = fitted_df)

    # Survival model (no intercept term)
    library(survival)
    test1 <- list(time = c(4,3,1,1,2,2,3),
                  status = c(1,1,1,0,1,1,0),
                  x = c(0,2,1,1,1,0,0),
                  sex = c(0,0,0,0,1,1,1))
    # Fit a stratified model
    m_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
    sim_coxph <- b_sim(m_coxph)
    ls_coxph <- linear_systematic(b_sims = sim_coxph,
                                  newdata = data.frame(x = 1))

    expect_is(ls_lm$ls_, 'numeric')
    expect_true(mean(ls_lm$ls_) < mean(ls_lm_no_intercept$ls_))
    expect_is(ls_coxph$ls_, 'numeric')
    expect_equal(names(ls_mpoly), c('education', 'typewc', 'I.education.2.',
                                    'ls_'))
    expect_equal(names(ls_mlog), c('typewc', 'log.education.', 'ls_'))
})

# Test qi_builder --------------------------------------------------------------
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
    linear_qi_slim2 <- qi_builder(m1, newdata = fitted_df, original_order = TRUE,
                                 slim = TRUE)

    linear_qi_auto_newdata <- qi_builder(m1)

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

# Test qi_slimmer --------------------------------------------------------------
test_that('qi_slimmer output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    fitted_df <- expand.grid(education = 6:16, type = 'wc')
    linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

    linear_slim <- qi_slimmer(linear_qi)

    expect_equal(nrow(linear_slim), 11)
    expect_equal(names(linear_slim), c('education', 'typewc', 'qi_min',
                                       'qi_median', 'qi_max'))
})


# Test interaction_builder -----------------------------------------------------
test_that('interaction_builder output validity', {
    states <- as.data.frame(state.x77)
    states$in.come <- states$Income
    m1 <- lm(Murder ~ Illiteracy + Income * Population, data = states)
    b_sims_1 <- b_sim(m1)

    m2 <- lm(Murder ~ in.come * Population, data = states)
    b_sims_2 <- b_sim(m2)

    m3 <- lm(Murder ~ in.come * Population * Frost, data = states)
    b_sims_3 <- b_sim(m3)

    fitted_df_1 <- expand.grid(Income = unique(states$Income), Illiteracy = 1,
                               Population = 4246)

    fitted_df_2 <- fitted_df_1
    fitted_df_2$Income.Population <- fitted_df_2$Income * fitted_df_2$Population

    fitted_df_3 <- expand.grid(in.come = unique(states$in.come),
                               Population = 4246, Frost = 104)

    inter_df_1 <- coreSim:::interaction_builder(b_sims_1, fitted_df_1)
    inter_df_2 <- coreSim:::interaction_builder(b_sims_1, fitted_df_2)
    inter_df_3 <- coreSim:::interaction_builder(b_sims_2, fitted_df_3)
    inter_df_4 <- coreSim:::interaction_builder(b_sims_3, fitted_df_3)

    inter_systematic <- linear_systematic(b_sims_1, fitted_df_1)

    expect_equal(inter_df_1$Income * inter_df_1$Population,
                 inter_df_1$Income.Population)
    expect_equal(inter_df_1$Income.Population, inter_df_2$Income.Population)
    expect_equal(inter_df_1$Income.Population, inter_df_3$in.come.Population)
    expect_equal(names(inter_df_4), c('in.come', 'Population', 'Frost',
                                      'in.come.Population', 'in.come.Frost',
                                      'Population.Frost',
                                      'in.come.Population.Frost'))
    expect_equal(ncol(inter_systematic), 5)


    fake_names <- c('a', 'b', 'c', 'd', 'e')
    interaction_5 <- coreSim:::possible_interaction_terms(fake_names, n = 5)

    expect_equal(length(interaction_5), 120)
    expect_error(coreSim:::possible_interaction_terms(fake_names, n = 6))
})

# Test ci_check --------------------------------------------------------------
test_that('ci_check output validity', {
    expect_equal(coreSim:::ci_check(95), 0.95)
    expect_equal(coreSim:::ci_check(1), 1)
    expect_equal(coreSim:::ci_check(0.5), 0.5)
    expect_error(coreSim:::ci_check(500))
})
