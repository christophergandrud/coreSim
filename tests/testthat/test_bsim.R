context('b_sim')

test_that('b_sim output validity', {
    set.seed(100)

    # Linear model
    data('Prestige', package = 'car')
    m1 <- lm(prestige ~ education + type, data = Prestige)
    m1_sims <- b_sim(m1)

    # Manually supplied coefficient means and covariance matrix
    coefs <- coef(m1)
    vcov_matrix <- vcov(m1)
    prestige_sims_manual <- b_sim(mu = coefs, Sigma = vcov_matrix)

    # Preference for obj

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
    expect_match(names(prestige_sims_manual)[[1]], 'intercept_')
    expect_message(b_sim(obj = m1, mu = coefs, Sigma = vcov_matrix),
                   'obj used rather than mu and/or Sigma')
    expect_match(names(sim_coxph)[[1]], 'x')
})
