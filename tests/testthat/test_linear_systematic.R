context('linear_systematic')

test_that('linear_systematic output validity', {
    set.seed(100)

    library(splines)

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

    mbs1 <- lm(prestige ~ bs(education) + type, data = Prestige)
    mbs1_sims <- b_sim(mbs1)
    ls_mbs1 <- linear_systematic(b_sims = mbs1_sims, newdata = fitted_df)

    mbs2 <- lm(prestige ~ bs(education, degree = 3) + type, data = Prestige)
    mbs2_sims <- b_sim(mbs2)
    ls_mbs2 <- linear_systematic(b_sims = mbs2_sims, newdata = fitted_df)

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
    expect_equal(names(ls_mbs1), c('typewc', 'bs.education.1', 'bs.education.2',
                                  'bs.education.3', 'ls_' ))
    expect_equal(names(ls_mbs2), c('typewc', 'bs.education..degree...3.1',
                                   'bs.education..degree...3.2',
                                   'bs.education..degree...3.3', 'ls_' ))
})
