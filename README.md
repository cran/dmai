# dmai: Divisia Monetary Aggregates Index


[![CRAN
release](https://www.r-pkg.org/badges/version-last-release/dmai)](https://CRAN.R-project.org/package=dmai)
[![CRAN
checks](https://badges.cranchecks.info/worst/dmai.svg)](https://cran.r-project.org/web/checks/check_results_dmai.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/dmai?color=green)](https://CRAN.R-project.org/package=dmai)
[![Monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/dmai?color=green)](https://CRAN.R-project.org/package=dmai)
[![r-universe](https://myaseen208.r-universe.dev/badges/dmai)](https://myaseen208.r-universe.dev/dmai)
[![R-CMD-check](https://github.com/myaseen208/dmai/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/myaseen208/dmai/actions/workflows/R-CMD-check.yml)
[![Development
version](https://img.shields.io/badge/devel%20version-1.0.0-orange.svg)](https://github.com/myaseen208/dmai)
[![Minimal R
version](https://img.shields.io/badge/R%3E%3D-4.2-6666ff.svg)](https://cran.r-project.org/)
[![Dependencies](https://tinyverse.netlify.app/badge/dmai)](https://CRAN.R-project.org/package=dmai)
[![License](https://img.shields.io/badge/License-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last
commit](https://img.shields.io/github/last-commit/myaseen208/dmai)](https://github.com/myaseen208/dmai/commits)

## Overview

**dmai** builds monetary aggregates from their components (currency,
demand deposits, savings and time deposits, …) by the methods of index
number theory:

- `dmai()`: the Divisia index of Barnett (1980), which weights every
  component by its share in the expenditure on monetary services priced
  by its user cost (Barnett 1978); also the variant with simple growth
  rates (Hancock), the Fisher ideal index, the simple sum and the
  currency equivalent index of Rotemberg, Driscoll and Poterba (1995),
  each with the dual user-cost price index;
- `dmai_compare()`: every method on the same data;
- `dmai_user_cost()`: the user costs and weights of the components;
- `dmai_moments()`: the Divisia second moments of Barnett and Serletis
  (1990);
- `ggplot2` graphics, a plain-language interpretation of every result
  (`dmai_interpret()`), the references of every method (`dmai_cite()`)
  and an application that runs all of this without code (`dmai_app()`).

Version 1.0.0 keeps the function, arguments and results of version
0.5.0, with one correction: the growth `DMAI` now uses natural
logarithms by default, so it is a growth rate in percent (0.5.0 used
base 10, which gives 0.434 times that; `logbase = 10` reproduces it).
The package no longer depends on the tidyverse, and its index formulas
reproduce the published values of the Consumer Price Index Manual (ILO
et al. 2004, Chapter 19).

## Installation

``` r
install.packages("dmai")
# development version
# install.packages("remotes")
remotes::install_github("myaseen208/dmai")
```

## Quick start

The example data `money_sim` are **simulated** monthly components of a
broad money aggregate with their rates and a benchmark rate (`rb`).

``` r
library(dmai)
data(money_sim)
fit <-
  dmai(
      .data     = money_sim
    , method    = "Barnett"
    , logbase   = NULL
    , date      = "Date"
    , x         = NULL
    , r         = NULL
    , user_cost = NULL
    , benchmark = "rb"
    , percent   = TRUE
    , base      = 100
    , as_dt     = FALSE
    )
fit
```

    Divisia (Barnett) index of 8 components, 240 dates
    ==================================================
    Dates: 2005-01-01 to 2024-12-01 (240); components: x1, x2, x3, x4, x5, x6, x7, x8
    Benchmark rate: column rb (upper envelope with the component rates)
    Growth (DMAI, log, percent): mean 0.8896, sd 0.5902, last 0.6266
    Index: 100 at the first date, 838.3 at the last; simple sum: 1,106

               Date    DMAI index value
      1: 2005-01-01      NA 100.0  5413
      2: 2005-02-01  0.8504 100.9  5459
      3: 2005-03-01  1.7329 102.6  5555
      4: 2005-04-01 -0.2494 102.4  5541
      5: 2005-05-01  0.3359 102.7  5560
     ---                               
    236: 2024-08-01  0.1823 804.2 43533
    237: 2024-09-01  0.5280 808.5 43764
    238: 2024-10-01  0.9856 816.5 44197
    239: 2024-11-01  2.0063 833.0 45093
    240: 2024-12-01  0.6266 838.3 45376

    Sources: Barnett (1978); Barnett (1980); Tornqvist (1936); Theil (1967). Full
    references: dmai_cite(x).

``` r
dmai_interpret(
    x   = fit
  , top = 3
  )
```

    Divisia (Barnett) monetary aggregate

    From 2005-01-01 to 2024-12-01 the Divisia (Barnett) aggregate grew 11.3% a
    year, against 12.8% for the simple sum (1.6 points less).

    Findings:
    - Largest growth at 2009-03-01, 2021-12-01, 2024-01-01 (2.49, 2.24, 2.17);
      smallest at 2007-06-01, 2012-09-01, 2018-12-01 (-0.71, -0.67, -0.61).
    - At the last date the largest weights are x3 (28.1%), x1 (19.5%), x2
      (18.8%).
    - The weights moved most for x1 (32.3 to 19.5%), x4 (4.3 to 9.9%), x6 (3.7 to
      8.5%).
    - The simple sum overstates money growth: it gives the same weight to
      interest-bearing deposits, which are held largely as savings, as to
      currency and demand deposits.

    Recommendations:
    - Compare the methods with dmai_compare(); check how the result depends on
      the benchmark rate.
    - dmai_moments() shows when the components grow at very different rates.

    Sources:
    - Barnett, W. A. (1978). The user cost of money. Economics Letters, 1(2),
      145-149. doi:10.1016/0165-1765(78)90051-4
    - Barnett, W. A. (1980). Economic monetary aggregates: an application of
      index number and aggregation theory. Journal of Econometrics, 14(1), 11-48.
      doi:10.1016/0304-4076(80)90070-6
    - Tornqvist, L. (1936). The Bank of Finland's consumption price index. Bank
      of Finland Monthly Bulletin, 10, 1-8.
    - Theil, H. (1967). Economics and Information Theory. North-Holland,
      Amsterdam.

``` r
plot(x = fit)
```

![](README_files/figure-commonmark/quick-plot-1.png)

## Learn more

- Vignettes: `browseVignettes("dmai")`, starting with
  `dmai_01_getting_started`; PDF versions on the site.
- Site: <https://myaseen208.com/dmai/>
- Notation: `dmai_notation()`
- References of the methods behind a result: `dmai_cite()`

## Citation

    Yaseen M, Nadeem A (2026). _dmai: Divisia Monetary Aggregates Index_. R
    package version 1.0.0 (first released 2018),
    <https://CRAN.R-project.org/package=dmai>.

BibTeX:

    @Manual{dmai,
      title = {{dmai}: Divisia Monetary Aggregates Index},
      author = {Muhammad Yaseen and Ahmad Nadeem},
      year = {2026},
      note = {R package version 1.0.0 (first released 2018)},
      url = {https://CRAN.R-project.org/package=dmai},
    }

Please also cite the methods you use: Barnett (1980) for the Divisia
index and Barnett (1978) for the user costs. `citation("dmai")` lists
them with the package, and `dmai_cite(x)` gives the references of a
single result.
