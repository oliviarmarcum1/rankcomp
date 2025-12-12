README
================

## What this package is for

`rankcomp` compares groups using rank-based methods when data may be
skewed, have outliers, or unequal variances. It provides all-pairs rank
tests (Steel–Dwass–Critchlow–Fligner), clear effect sizes (probability
of superiority), and simple checks so users can decide whether two
groups are not just different but close enough to count as the same in
practice via equivalence/non-inferiority. Outputs are tidy tables for
easy reporting.

## Installation

<<<<<<< HEAD
``` r
# install.packages("remotes")
remotes::install_github("oliviarmarcum1/rankcomp")
```

## Minimal example

``` r
=======
```r 
# install.packages(“remotes”)

remotes::install_github(“oliviarmarcum1/rankcomp”)
```

## Minimal example
```r
>>>>>>> 0963f87da8115c71a15727e2687ee7d287ec5521
library(rankcomp)
set.seed(1)
x <- c(rnorm(10,0), rnorm(12,0.6), rnorm(9,0))
g <- factor(rep(c("A","B","C"), c(10,12,9)))

pairwise_rank_sum(x, g)
np_effect_size(x, g, measure = "auc", ci = TRUE, nboot = 200, seed = 1)
equivalence_np(x, g, delta = 0.05, measure = "auc",
               alternative = "equivalence", nboot = 200, seed = 1)
```
