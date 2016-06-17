![coreSim logo](img/coreSim_logo.png)

Version: 0.2 [![Build Status](https://travis-ci.org/christophergandrud/coreSim.svg?branch=master)](https://travis-ci.org/christophergandrud/coreSim)
[![codecov.io](https://codecov.io/github/christophergandrud/coreSim/coverage.svg?branch=master)](https://codecov.io/github/christophergandrud/coreSim?branch=master)

> Core functionality for simulating quantities of interest from generalised 
linear models.

# Purpose

Using simulations to find quantities of interest and associated uncertainty
can be an effective way of showing substantively meaningful results from 
generalised linear models (GLM). 

This R package provides core functions that can serve as the **backbone** to other
packages for finding and plotting simulated quantities of interest from GLMs.

# Motivation

**coreSim** aims to solve a number of issues that arose in prior implementations of 
the simulation approach to showing GLM results. The main previous implementation in R is the **Zelig** package. This package has tried to be "Everyone's statistical software". However, paradoxically, its attempt to be everything to everyone has led to less flexibility for new use cases. Maintaining such a large project over time has led to (in my experience) frequent code breaks. The Zelig 'API' has changed considerably over time in often undocumented ways. Changes to its many dependencies also undermines its reliability. 

**coreSim** aims to overcome these issue with a focus on *simplicity*. It tries to:

- Do a few things really well.

- Have as few dependencies as possible. Only import packages if they make significant performance improvements over what is available in base R.

- Return simple `data.frame` output. 

- Have informative error messages that are easy for users to understand.

Additionally, **coreSim** is aiming for very high reliability. Simplicity helps with this, so does aiming for 100% code test coverage.

These characteristics allow **coreSim** to form the backbone of many specific and unanticipated implementations of the simulation approach. 

# Steps

1. Estimate your model using whatever GLM model fitting function you like 
(note: I've only tested `lm`, `glm`, and `survival`).

2. Find your quantities of interest with `qi_builder`.

3. Present your results, e.g. by plotting the simulated quantities of 
interest.

# Examples

## Normal linear model 

Here is an example using data from the **car** package:


```r
library(coreSim)
library(car)

# Normal linear model
m1 <- lm(prestige ~ education + type, data = Prestige)

# Create fitted values
fitted_df_1 <- expand.grid(education = 6:16, type = 'wc')

# Find predicted outcomes (95% central interval, by default)
linear_qi <- qi_builder(obj = m1, newdata = fitted_df_1)
```

```
## Note: FUN argument missing -> assuming b_sims is from a normal linear model.
```

```r
head(linear_qi)
```

```
##   education typewc      qi_
## 1         6      1 18.66254
## 2         6      1 16.38486
## 3         6      1 15.09804
## 4         6      1 17.22725
## 5         6      1 16.97208
## 6         6      1 16.44157
```

### Slimmed simulation data

By default `qi_builder` will return all of the simulations inside the central 
interval of the simulations for each scenario that you specify with the `ci` 
argument (this is `0.95` by default for 95% central interval). 

However, you may want to only return key features of this interval so that they
can be efficiently stored and plotted. Using `slim = TRUE` will return only the
minimum, median, and maximum values of the central interval for each scenario:


```r
linear_qi_slim <- qi_builder(m1, newdata = fitted_df_1, slim = TRUE)

head(linear_qi_slim)
```

```
##   education typewc   qi_min qi_median   qi_max
## 1         6      1 12.09169  19.07343 26.66386
## 2         7      1 17.84964  23.70924 30.22625
## 3         8      1 23.52470  28.31701 33.86071
## 4         9      1 28.87492  32.94063 37.54575
## 5        10      1 34.14692  37.48935 41.16243
## 6        11      1 39.00953  42.04769 45.35880
```

The slimmed simulation data set can be efficiently plotted, for example using
[ggplot2](http://docs.ggplot2.org/current/):


```r
library(ggplot2)
theme_set(theme_bw())

ggplot(linear_qi_slim, aes(education, qi_median)) +
    geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3) +
    geom_line() +
    ylab('Prestige')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

## Predicted probabilities from logistic regressions

By default `qi_builder` simply returns the linear systematic component, which 
in normal linear regression is simply the predicted y, i.e. 
$y = \alpha + \mathrm{\beta X}$.

`qi_builder` allows you to supply any function for creating quantities of 
interest that you would like. This function needs to simply be able to convert
a vector of linear systematic components to your quantity of interest. 

For example, to find predicted probabilities from a logistic regression model
create a function to turn the systematic component into the QI:


```r
pr_fun <- function(x) 1 / (1 + exp(-x))
```

Then supply the custom function to `qi_builder`'s `FUN` argument:


```r
# Download data
URL <- 'http://www.ats.ucla.edu/stat/data/binary.csv'
Admission <- read.csv(URL)
Admission$rank <- as.factor(Admission$rank)

# Estimate model
m2 <- glm(admit ~ gre + gpa + rank, data = Admission, family = 'binomial')

# Create fitted values
m2_fitted <- expand.grid(gre = seq(220, 800, by = 10), gpa = c(1, 4), rank = '4')

# Find quantity of interest
logistic_qi <- qi_builder(m2, m2_fitted, FUN = pr_fun, slim = TRUE)

head(logistic_qi)
```

```
##   gre gpa rank4      qi_min  qi_median     qi_max
## 1 220   1     1 0.002795462 0.01377568 0.06626129
## 2 230   1     1 0.002891496 0.01414194 0.06715291
## 3 240   1     1 0.002990819 0.01447833 0.06742138
## 4 250   1     1 0.003086639 0.01474815 0.06938533
## 5 260   1     1 0.003163867 0.01509728 0.07069639
## 6 270   1     1 0.003227475 0.01541323 0.07110850
```



# See also

Christopher Gandrud (2015). simPH: An R Package for Illustrating Estimates from
Cox Proportional Hazard Models Including for Interactive and Nonlinear Effects.
Journal of Statistical Software, 65(3), 1-20.
<http://www.jstatsoft.org/v65/i03/>.

Gandrud, Christopher. Laron K. Williams and Guy D. Whitten (2015). dynsim: Dynamic
Simulations of Autoregressive Relationships. R package version 1.2.1.
<https://CRAN.R-project.org/package=dynsim>.

King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the Most of 
Statistical Analyses: Improving Interpretation and Presentation." American 
Journal of Political Science 44(2): 341-55.

