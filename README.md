
# PDXpower

<!-- badges: start -->

[![metacran
downloads](https://cranlogs.r-pkg.org/badges/PDXpower)](https://cran.r-project.org/package=PDXpower)
[![](https://cranlogs.r-pkg.org/badges/grand-total/PDXpower)](https://cran.r-project.org/package=PDXpower)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/PDXpower)](https://cran.r-project.org/package=PDXpower)
[![CRAN_Status_Badge_version_last_release](https://www.r-pkg.org/badges/version-last-release/PDXpower)](https://cran.r-project.org/package=PDXpower)
<!-- badges: end -->

The `PDXpower` package can conduct power analysis for time-to-event
outcome based on empirical simulations.

## Installation

You can install the development version of `PDXpower` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shanpengli/PDXpower")
```

## Example

Below is a toy example how to conduct power analysis based on a
preliminary dataset `animals1`. Particularly, we need to specify a
formula that fits a ANOVA mixed effects model with correlating variables
in `animals1`, where `ID` is the PDX line number, `Y` is the event time
variable, and `Tx` is the treatment variable.

Next, run power analysis by fitting a ANOVA mixed effects model on
`animals1`.

``` r
library(PDXpower)
#> Loading required package: survival
#> Loading required package: parallel
data(animals1)
### Power analysis on a preliminary dataset by assuming the time to event is log-normal
PowTab <- PowANOVADat(data = animals1, formula = log(Y) ~ Tx, 
                      random = ~ 1|ID, n = c(3, 5, 10), m = c(2, 3, 4), sim = 100)
PowTab
#> Parameter estimates based on the pilot data:
#> Treatment effect (beta): 0.7299 
#> Variance of random effect (tau2): 0.0332 
#> Random error variance (sigma2): 0.386 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination of n and m).
#> 
#>    n m  N Power (%)
#> 1  3 2 12        43
#> 2  3 3 18        65
#> 3  3 4 24        77
#> 4  5 2 20        70
#> 5  5 3 30        87
#> 6  5 4 40        95
#> 7 10 2 40        98
#> 8 10 3 60        99
#> 9 10 4 80       100
```

The following code generates a power curve based on the object `PowTab`.

``` r
plotpower(PowTab[[4]], ylim = c(0, 1))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Or we can fit a ANOVA fixed effect model for running power analysis.

``` r
PowTabfit <- PowANOVADat(data = animals1, formula = log(Y) ~ Tx, 
                      random = ~ 1|ID, n = c(3, 5, 10), m = c(2, 3, 4), fixed.effect = TRUE, sim = 100)
PowTabfit
#> Parameter estimates based on the pilot data:
#> Treatment effect (beta): 0.7299 
#> Variance of random effect (tau2): 0.0332 
#> Random error variance (sigma2): 0.386 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination of n and m).
#> 
#>    n m  N Power (%) for fixed effects
#> 1  3 2 12                          22
#> 2  3 3 18                          37
#> 3  3 4 24                          36
#> 4  5 2 20                          31
#> 5  5 3 30                          49
#> 6  5 4 40                          59
#> 7 10 2 40                          58
#> 8 10 3 60                          83
#> 9 10 4 80                          89
```

Alternatively, one can run power analysis by fitting a Cox frailty
model. Here we present another dataset `animals2`. Particularly, we need
to specify a formula that fits a Cox frailty model with correlating
variables in `animals2`, where `ID` is the PDX line number, `Y` is the
event time variable, `Tx` is the treatment variable, and `status` is the
event status.

``` r
data(animals2)
### Power analysis on a preliminary dataset by assuming the time to event is Weibull-distributed
PowTab <- PowFrailtyDat(data = animals2, formula = Surv(Y, status) ~ Tx + cluster(ID), 
                        n = c(3, 5, 10), m = c(2, 3, 4), sim = 100)
PowTab
#> Parameter estimates based on the pilot data:
#> Scale parameter (lambda): 0.0154 
#> Shape parameter (nu): 2.1722 
#> Treatment effect (beta): -0.8794 
#> Variance of random effect (tau2): 0.0422 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination
#>   of n and m).
#> The mean censoring rate for each combination of n and m is calculated across  100  Monte Carlo samples.
#> 
#>    n m  N Power (%) for Cox frailty Censoring Rate
#> 1  3 2 12                     36.49              0
#> 2  3 3 18                     42.35              0
#> 3  3 4 24                     62.22              0
#> 4  5 2 20                     49.30              0
#> 5  5 3 30                     69.23              0
#> 6  5 4 40                     78.72              0
#> 7 10 2 40                     90.57              0
#> 8 10 3 60                     88.78              0
#> 9 10 4 80                     96.91              0
```

The following code generates a power curve based on the object `PowTab`.

``` r
plotpower(PowTab[[5]], ylim = c(0, 1))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Or we can fit a Cox fixed effect model for running power analysis.

``` r
PowTab <- PowFrailtyDat(data = animals2, formula = Surv(Y, status) ~ Tx + cluster(ID), 
                        n = c(3, 5, 10), m = c(2, 3, 4), fixed.effect = TRUE, sim = 100)
PowTab
#> Parameter estimates based on the pilot data:
#> Scale parameter (lambda): 0.0154 
#> Shape parameter (nu): 2.1722 
#> Treatment effect (beta): -0.8794 
#> Variance of random effect (tau2): 0.0422 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination
#>   of n and m).
#> The mean censoring rate for each combination of n and m is calculated across  100  Monte Carlo samples.
#> 
#>    n m  N Power (%) for Cox fixed effects Censoring Rate
#> 1  3 2 12                              36              0
#> 2  3 3 18                              48              0
#> 3  3 4 24                              65              0
#> 4  5 2 20                              57              0
#> 5  5 3 30                              71              0
#> 6  5 4 40                              83              0
#> 7 10 2 40                              90              0
#> 8 10 3 60                              92              0
#> 9 10 4 80                              97              0
```

Alternatively, we may also conduct power analysis based on median
survival of two randomized arms. We suppose that the median survival of
the control and treatment arm is 2.4 and 4.8, allowing a PDX line has
10% marginal error (`tau2=0.1`) of treatment effect and an exponential
event time, a power analysis may be done as below:

``` r
### Assume the time to event outcome is weibull-distributed
PowTab <- PowFrailty(ctl.med.surv = 2.4, tx.med.surv = 4.8, nu = 1, tau2 = 0.1, sim = 100,
                     n = c(3, 5, 10), m = c(2, 3, 4))
#> Treatment effect (beta): -0.6931472 
#> Scale parameter (lambda): 0.2888113 
#> Shape parameter (nu): 1 
#> Variance of random effect (tau2): 0.1 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination
#>   of n and m).
#> The mean censoring rate for each combination of n and m is calculated across  100  Monte Carlo samples.
#> 
#>    n m  N Power (%) for Cox frailty Censoring Rate
#> 1  3 2 12                     22.45              0
#> 2  3 3 18                     21.05              0
#> 3  3 4 24                     41.41              0
#> 4  5 2 20                     35.16              0
#> 5  5 3 30                     44.79              0
#> 6  5 4 40                     62.89              0
#> 7 10 2 40                     67.01              0
#> 8 10 3 60                     73.74              0
#> 9 10 4 80                     86.73              0
```

``` r
### Assume the time to event outcome is log-normal
PowTab <- PowANOVA(ctl.med.surv = 2.4, tx.med.surv = 4.8, icc = 0.25, sigma2 = 1, sim = 100,
                   n = c(3, 5, 10), m = c(2, 3, 4))
#> Treatment effect (beta): -0.6931472 
#> Variance of random effect (tau2): 0.3333333 
#> Intraclass correlation coefficient (icc): 0.25 
#> Random error variance (sigma2): 1 
#> Monte Carlo power estimate, calculated as the
#>   proportion of instances where the null hypothesis
#>   H_0: beta = 0 is rejected (n = number of PDX lines,
#>   m = number of animals per arm per PDX line,
#>   N = total number of animals for a given combination
#>   of n and m).
#> 
#>    n m  N Power (%)
#> 1  3 2 12        25
#> 2  3 3 18        30
#> 3  3 4 24        36
#> 4  5 2 20        32
#> 5  5 3 30        47
#> 6  5 4 40        58
#> 7 10 2 40        60
#> 8 10 3 60        73
#> 9 10 4 80        87
```
