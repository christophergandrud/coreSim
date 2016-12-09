context('interaction_builder')

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
