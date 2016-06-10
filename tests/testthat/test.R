# Test b_sim -------------------------------------------------------------------
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

test_that('b_sim output validity', {
        expect_equal(round(sum(m1_sims[['intercept_']])), -2605)
        expect_equal(round(sum(m1_sims[['typeprof']])), 6199)
        expect_match(names(sim_coxph)[[1]], 'x')
})

# Test linear_systematic -------------------------------------------------------
# Linear model
fitted_df <- expand.grid(education = 6:16, typewc = 1)
ls_lm <- linear_systematic(b_sims = m1_sims, newdata = fitted_df)

# Survival model
ls_coxph <- linear_systematic(sim_coxph, newdata = data.frame(x = 1))

test_that('linear_systematic output validity', {
    expect_equal(round(sum(ls_lm$ls_)), 492460)
    expect_equal(round(sum(ls_coxph$ls_)), 800)
})

# Test qi_builder -------------------------------------------------------

# Linear model
linear_qi <- qi_builder(b_sims = m1_sims, newdata = fitted_df)

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

test_that('qi_builder output validity', {
    expect_equal(round(sum(linear_qi$qi_)), 468114)
    expect_equal(round(sum(logistic_qi)), 57639730)
})
