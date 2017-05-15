<!-- README.md is generated from README.Rmd. Please edit that file -->
![coreSim logo](img/coreSim_logo.png)

Version: 0.2.4 [![CRAN
Version](http://www.r-pkg.org/badges/version/coreSim)](http://cran.r-project.org/package=coreSim)
[![Build
Status](https://travis-ci.org/christophergandrud/coreSim.svg?branch=master)](https://travis-ci.org/christophergandrud/coreSim)
[![codecov.io](https://codecov.io/github/christophergandrud/coreSim/coverage.svg?branch=master)](https://codecov.io/github/christophergandrud/coreSim?branch=master)
![CRAN Monthly
Downloads](http://cranlogs.r-pkg.org/badges/last-month/coreSim) ![CRAN
Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/coreSim)

> Core functionality for simulating quantities of interest from
> generalised linear models.

Purpose
=======

Using simulations to find quantities of interest and associated
uncertainty can be an effective way of showing substantively meaningful
results from generalised linear models (GLM).

This R package provides core functions that can serve as the
**backbone** to other packages for finding and plotting simulated
quantities of interest from GLMs.

**coreSim** currently powers
[pltesim](https://github.com/christophergandrud/pltesim), a package for
simulating probabilistic long-term effects from models with temporal
dependence.

Motivation
==========

**coreSim** aims to solve a number of issues that arose in prior
implementations of the simulation approach to showing GLM results. The
main previous implementation in R is the **Zelig** package. This package
has tried to be "Everyone's statistical software". However,
paradoxically, its attempt to be everything to everyone has led to less
flexibility for new use cases. Maintaining such a large project over
time has led to (in my experience) frequent code breaks. The Zelig 'API'
has changed considerably over time in often undocumented ways. Changes
to its many dependencies also undermines its reliability.

**coreSim** aims to overcome these issue with a focus on *simplicity*.
It tries to:

-   Do a small set of things really well.

-   Have as few dependencies as possible. Only import packages if they
    make significant performance improvements over base R.

-   Return simple `data.frame` output that can be easily manipulated.

-   Have informative error messages that are easy for users to
    understand and which guide them to solutions.

Additionally, **coreSim** aims for very high reliability. Simplicity
helps with acheive this goal. So does aiming for 100% code test
coverage.

These characteristics allow **coreSim** to form the backbone of many
specific and unanticipated implementations of the simulation approach.

 Steps
======

1.  Estimate your model using whatever GLM model fitting function you
    like (note: I've only tested `lm`, `glm`, and `survival`).

2.  Find your quantities of interest with `qi_builder`.

3.  Present your results, e.g. by plotting the simulated quantities of
    interest.

Examples
========

Normal linear model
-------------------

Here is an example using data from the **car** package:

    library(coreSim)
    library(car)

    # Normal linear model
    m1 <- lm(prestige ~ education + type, data = Prestige)

    # Create fitted values
    fitted_df_1 <- expand.grid(education = 6:16, type = 'wc')

    # Find predicted outcomes (95% central interval, by default)
    linear_qi <- qi_builder(obj = m1, newdata = fitted_df_1)

    ## Note: FUN argument missing -> assuming b_sims is from a normal linear model.

    head(linear_qi)

    ##   education typewc      qi_
    ## 1         6      1 18.26931
    ## 2         6      1 20.92067
    ## 3         6      1 20.88659
    ## 4         6      1 24.46231
    ## 5         6      1 19.15713
    ## 6         6      1 17.91391

### Slimmed simulation data

By default `qi_builder` will return all of the simulations inside the
central interval of the simulations for each scenario that you specify
with the `ci` argument (this is `0.95` by default for 95% central
interval).

However, you may want to only return key features of this interval so
that they can be efficiently stored and plotted. Using `slim = TRUE`
will return only the minimum, median, and maximum values of the central
interval for each scenario:

    linear_qi_slim <- qi_builder(m1, newdata = fitted_df_1, slim = TRUE)

    head(linear_qi_slim)

    ##   education typewc   qi_min qi_median   qi_max
    ## 1         6      1 12.54993  19.33749 26.46225
    ## 2         7      1 18.10320  23.88602 29.90101
    ## 3         8      1 23.71457  28.48066 33.52652
    ## 4         9      1 29.04148  33.05269 36.90274
    ## 5        10      1 34.09772  37.64164 41.08075
    ## 6        11      1 38.99077  42.18919 45.31262

The slimmed simulation data set can be efficiently plotted, for example
using [ggplot2](http://docs.ggplot2.org/current/):

    library(ggplot2)
    theme_set(theme_bw())

    ggplot(linear_qi_slim, aes(education, qi_median)) +
        geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3) +
        geom_line() +
        ylab('Prestige')

![](man/figures/unnamed-chunk-3-1.png)

Predicted probabilities from logistic regressions
-------------------------------------------------

By default `qi_builder` simply returns the linear systematic component,
which in normal linear regression is simply the predicted y, i.e.
*y* = *α* + *β**X*.

`qi_builder` allows you to supply any function for creating quantities
of interest that you would like. This function needs to simply be able
to convert a vector of linear systematic components to your quantity of
interest.

For example, to find predicted probabilities from a logistic regression
model create a function to turn the systematic component into the QI:

    pr_fun <- function(x) 1 / (1 + exp(-x))

Then supply the custom function to `qi_builder`'s `FUN` argument:

    # Load data
    data(Admission)
    Admission$rank <- as.factor(Admission$rank)

    # Estimate model
    m2 <- glm(admit ~ gre + gpa + rank, data = Admission, family = 'binomial')

    # Create fitted values
    m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(1, 4), rank = '4')

    # Find quantity of interest
    logistic_qi <- qi_builder(m2, m2_fitted, FUN = pr_fun, slim = TRUE)

    head(logistic_qi)

    ##   gre gpa rank4      qi_min  qi_median     qi_max
    ## 1 220   1     1 0.002879333 0.01431348 0.06336300
    ## 2 230   1     1 0.002983825 0.01471619 0.06436528
    ## 3 240   1     1 0.003106020 0.01501327 0.06449270
    ## 4 250   1     1 0.003205407 0.01537888 0.06520055
    ## 5 260   1     1 0.003301195 0.01566664 0.06620142
    ## 6 270   1     1 0.003415190 0.01588294 0.06787158

See also
========

Christopher Gandrud (2015). simPH: An R Package for Illustrating
Estimates from Cox Proportional Hazard Models Including for Interactive
and Nonlinear Effects. Journal of Statistical Software, 65(3), 1-20.
<http://www.jstatsoft.org/v65/i03/>.

Gandrud, Christopher. Laron K. Williams and Guy D. Whitten (2015).
dynsim: Dynamic Simulations of Autoregressive Relationships. R package
version 1.2.1. <https://CRAN.R-project.org/package=dynsim>.

King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the Most
of Statistical Analyses: Improving Interpretation and Presentation."
American Journal of Political Science 44(2): 341-55.
