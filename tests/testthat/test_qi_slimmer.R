context('qi_slimmer')

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
