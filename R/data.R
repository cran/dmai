#' Simulated monetary components and rates
#'
#' @description
#' SIMULATED monthly data, January 2005 to December 2024, shaped like the components of a broad money aggregate of
#' a central bank: quantities (billions) and rates of return (percent a year) of eight components, and a benchmark
#' rate. The numbers are not real; they are drawn by \code{data-raw/make-data.R} in the source package (rates as
#' autoregressive processes that follow a policy-type rate, quantities as trending series that respond to the
#' spread between their own rate and the benchmark) to show the methods.
#'
#' @format A data frame with 240 rows and 18 columns:
#' \describe{
#'   \item{\code{Date}}{first day of the month (class \code{Date}).}
#'   \item{\code{x1}}{currency in circulation.}
#'   \item{\code{x2}}{demand deposits.}
#'   \item{\code{x3}}{savings deposits.}
#'   \item{\code{x4}}{time deposits up to 3 months.}
#'   \item{\code{x5}}{time deposits of 3 to 6 months.}
#'   \item{\code{x6}}{time deposits of 6 months to 1 year.}
#'   \item{\code{x7}}{time deposits over 1 year.}
#'   \item{\code{x8}}{foreign currency deposits.}
#'   \item{\code{r1}}{the own rate of \code{x1} in percent (zero).}
#'   \item{\code{r2}}{the own rate of \code{x2} in percent (zero).}
#'   \item{\code{r3}}{the own rate of \code{x3} in percent.}
#'   \item{\code{r4}}{the own rate of \code{x4} in percent.}
#'   \item{\code{r5}}{the own rate of \code{x5} in percent.}
#'   \item{\code{r6}}{the own rate of \code{x6} in percent.}
#'   \item{\code{r7}}{the own rate of \code{x7} in percent.}
#'   \item{\code{r8}}{the own rate of \code{x8} in percent.}
#'   \item{\code{rb}}{the benchmark rate in percent (a long bond yield), above every component rate.}
#' }
#' @source Simulated; see \code{data-raw/make-data.R}.
#' @family dmai data
#' @examples
#' data(money_sim)
#' head(money_sim)
"money_sim"

#' Artificial price and quantity data of the Consumer Price Index Manual
#'
#' @description
#' The artificial data set of Chapter 19 of the Consumer Price Index Manual (ILO et al. 2004, Tables 19.1 and
#' 19.2): prices and quantities of six commodities in five periods. The Manual gives the chained Laspeyres,
#' Paasche, Fisher and Tornqvist price indexes of these data (Tables 19.5 and 19.11), which the package reproduces:
#' a Divisia quantity index is a Tornqvist index with the user costs as prices, so \code{\link{dmai}()} with the
#' prices as \code{x} and the quantities as \code{user_cost} gives the Manual's price indexes (the two roles are
#' symmetric in these formulas).
#'
#' @format A data frame with 5 rows and 13 columns: \code{period} (1 to 5), \code{p1}, \dots, \code{p6} (prices)
#'   and \code{q1}, \dots, \code{q6} (quantities).
#' @source ILO, IMF, OECD, UNECE, Eurostat and The World Bank (2004). \emph{Consumer Price Index Manual: Theory and
#'   Practice}. International Labour Office, Geneva. Chapter 19, Tables 19.1 and 19.2.
#' @family dmai data
#' @examples
#' data(cpi_ch19)
#' # the chained Tornqvist price index of Table 19.11: 1, 1.4052, 1.3112, 1.2624, 1.2224
#' dmai(
#'     .data     = cpi_ch19
#'   , method    = "Barnett"
#'   , logbase   = NULL
#'   , date      = "period"
#'   , x         = paste0("p", 1:6)
#'   , r         = NULL
#'   , user_cost = paste0("q", 1:6)
#'   , benchmark = NULL
#'   , percent   = TRUE
#'   , base      = 1
#'   , as_dt     = TRUE
#'   )
"cpi_ch19"
