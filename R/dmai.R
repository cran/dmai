#' Divisia monetary aggregates index
#'
#' @description
#' \code{dmai()} aggregates monetary assets (currency, deposits, ...) into one index of monetary services. Each
#' component is weighted by its share in the expenditure on monetary services, priced by its user cost (Barnett
#' 1978, 1980): an asset that pays a rate close to the benchmark rate is held mostly as a store of value and gets a
#' small weight, while currency and demand deposits get a large one. The simple sum, which weights every component
#' equally, and the currency equivalent index are given for comparison.
#'
#' @details
#' With quantities \eqn{x_{it}}{x_it} of components \eqn{i = 1, \dots, n}{i = 1, ..., n} at dates \eqn{t}{t}, their
#' rates of return \eqn{r_{it}}{r_it} and the benchmark rate \eqn{R_t}{R_t}, the user cost of component \eqn{i}{i}
#' is \deqn{\pi_{it} = \frac{R_t - r_{it}}{1 + R_t}}{pi_it = (R_t - r_it) / (1 + R_t)} (Barnett 1978) and its
#' expenditure share \eqn{s_{it} = \pi_{it}x_{it} / \sum_j \pi_{jt}x_{jt}}{s_it = pi_it x_it / sum_j pi_jt x_jt}.
#' With \eqn{\bar{s}_{it} = (s_{it} + s_{i,t-1})/2}{sbar_it = (s_it + s_i,t-1) / 2}, the methods are
#' \describe{
#'   \item{\code{"Barnett"}}{the Divisia index in the discrete form of Tornqvist and Theil (Barnett 1980):
#'     \deqn{\log\frac{Q_t}{Q_{t-1}} = \sum_i \bar{s}_{it}\left(\log x_{it} - \log x_{i,t-1}\right).}{log(Q_t /
#'     Q_t-1) = sum_i sbar_it (log x_it - log x_i,t-1).}}
#'   \item{\code{"Hancock"}}{the same shares with simple growth rates (as in version 0.5.0):
#'     \deqn{\frac{Q_t}{Q_{t-1}} - 1 = \sum_i \bar{s}_{it}\left(\frac{x_{it}}{x_{i,t-1}} - 1\right).}{Q_t / Q_t-1 -
#'     1 = sum_i sbar_it (x_it / x_i,t-1 - 1).}}
#'   \item{\code{"Fisher"}}{the Fisher ideal index, the geometric mean of the Laspeyres and Paasche indexes with the
#'     user costs as prices (Fisher 1922; Diewert 1976): \deqn{\frac{Q_t}{Q_{t-1}} = \left(\frac{\sum_i
#'     \pi_{i,t-1}x_{it}}{\sum_i \pi_{i,t-1}x_{i,t-1}}\cdot\frac{\sum_i \pi_{it}x_{it}}{\sum_i
#'     \pi_{it}x_{i,t-1}}\right)^{1/2}.}{Q_t / Q_t-1 = sqrt(L_t P_t), L and P the Laspeyres and Paasche links.}}
#'   \item{\code{"SimpleSum"}}{the official aggregate \eqn{\sum_i x_{it}}{sum_i x_it}.}
#'   \item{\code{"CE"}}{the currency equivalent index \eqn{\sum_i (R_t - r_{it})x_{it}/R_t}{sum_i (R_t - r_it)
#'     x_it / R_t} (Rotemberg, Driscoll and Poterba 1995; Barnett 1991).}
#' }
#' All indexes are chained and equal \code{base} at the first date. The growth \code{DMAI} is
#' \eqn{100\log(Q_t/Q_{t-1})/\log(\mathrm{logbase})}{100 log(Q_t / Q_t-1) / log(logbase)}, in percent for the
#' natural logarithm, and \eqn{100(Q_t/Q_{t-1} - 1)}{100 (Q_t / Q_t-1 - 1)} for \code{"Hancock"}.
#'
#' The benchmark rate is the largest rate of the components at each date when \code{benchmark = NULL} (as in
#' version 0.5.0; that component then has a user cost of zero), or the given benchmark, raised to the largest
#' component rate where it is lower (the upper envelope). The factor \eqn{1/(1 + R_t)}{1 / (1 + R_t)} is common to
#' the components and does not change the shares.
#'
#' The dual user-cost price index follows by factor reversal, \eqn{P_t = E_t / V_t}{P_t = E_t / V_t}, with the
#' expenditure \eqn{E_t = \sum_i \pi_{it}x_{it}}{E_t = sum_i pi_it x_it} and the aggregate \eqn{V_t}{V_t} in the
#' units of \eqn{x}{x} (column \code{value}).
#'
#' @param .data a data frame with one row per date: a date column and, for every component, a quantity column and a
#'   rate column (named \code{x1}, \code{x2}, ... and \code{r1}, \code{r2}, ... unless \code{x} and \code{r} are
#'   given).
#' @param method \code{"Barnett"} (default), \code{"Hancock"}, \code{"Fisher"}, \code{"SimpleSum"} or \code{"CE"}.
#' @param logbase base of the logarithm of the growth \code{DMAI} of the \code{"Barnett"}, \code{"Fisher"},
#'   \code{"SimpleSum"} and \code{"CE"} indexes; \code{NULL} (default) for the natural logarithm. Version 0.5.0 used
#'   base 10 by default; \code{logbase = 10} reproduces it.
#' @param date name of the date column (default \code{"Date"}).
#' @param x names of the quantity columns (default \code{NULL}: \code{x1}, \code{x2}, ...).
#' @param r names of the rate columns, in the order of \code{x} (default \code{NULL}: \code{r1}, \code{r2}, ...).
#' @param user_cost names of columns that give the user costs directly, in the order of \code{x} (default
#'   \code{NULL}: computed from the rates). The rates are then needed only for \code{"CE"}.
#' @param benchmark the benchmark rate: \code{NULL} (default, the largest component rate), a column name, a number
#'   or one value per row of \code{.data}, in the units of the rates.
#' @param percent logical; are the rates in percent (default \code{TRUE}) or decimal?
#' @param base value of the index at the first date (default 100).
#' @param as_dt logical; \code{TRUE} (default) returns the table of the index, as version 0.5.0 did; \code{FALSE}
#'   returns the full result.
#' @return With \code{as_dt = TRUE}, a \code{data.table} with the date, \code{DMAI} (the growth in percent,
#'   \code{NA} at the first date), \code{index} and \code{value} (the index in the units of \code{x}, equal to the
#'   simple sum at the first date). With \code{as_dt = FALSE}, an object of class \code{c("dmai_index",
#'   "dmai_result", "list")} with \code{index} (that table), \code{components} (quantities, rates, user costs and
#'   shares by date), \code{benchmark}, \code{price} (expenditure and dual price index), \code{moments} (see
#'   \code{\link{dmai_moments}()}), \code{simple_sum}, \code{fisher} (Laspeyres and Paasche links, for
#'   \code{"Fisher"}) and \code{settings}. \code{print()}, \code{plot()}, \code{ggplot2::autoplot()} and
#'   \code{\link{dmai_interpret}()} apply.
#' @references
#' Barnett, W. A. (1978). The user cost of money. \emph{Economics Letters}, \strong{1}(2), 145--149.
#' \doi{10.1016/0165-1765(78)90051-4}
#'
#' Barnett, W. A. (1980). Economic monetary aggregates: an application of index number and aggregation theory.
#' \emph{Journal of Econometrics}, \strong{14}(1), 11--48. \doi{10.1016/0304-4076(80)90070-6}
#'
#' Diewert, W. E. (1976). Exact and superlative index numbers. \emph{Journal of Econometrics}, \strong{4}(2),
#' 115--145. \doi{10.1016/0304-4076(76)90009-9}
#'
#' Hancock, D. (1985). The financial firm: production with monetary and nonmonetary goods. \emph{Journal of
#' Political Economy}, \strong{93}(5), 859--880. \doi{10.1086/261339}
#'
#' Rotemberg, J. J., Driscoll, J. C. and Poterba, J. M. (1995). Money, output, and prices: evidence from a new
#' monetary aggregate. \emph{Journal of Business and Economic Statistics}, \strong{13}(1), 67--83.
#' @author
#' Muhammad Yaseen (\email{myaseen208@@gmail.com}) and Ahmad Nadeem (\email{Ahmed.Nadeem@@sbp.org.pk})
#' @family dmai analyses
#' @seealso \code{\link{dmai_compare}}, \code{\link{dmai_user_cost}}, \code{\link{dmai_moments}},
#'   \code{\link{dmai_interpret}}, \code{\link{money_sim}}
#' @examples
#' data(money_sim)
#' d <-
#'   dmai(
#'       .data     = money_sim
#'     , method    = "Barnett"
#'     , logbase   = NULL
#'     , date      = "Date"
#'     , x         = NULL
#'     , r         = NULL
#'     , user_cost = NULL
#'     , benchmark = "rb"
#'     , percent   = TRUE
#'     , base      = 100
#'     , as_dt     = TRUE
#'     )
#' d
#' fit <-
#'   dmai(
#'       .data     = money_sim
#'     , method    = "Barnett"
#'     , logbase   = NULL
#'     , date      = "Date"
#'     , x         = NULL
#'     , r         = NULL
#'     , user_cost = NULL
#'     , benchmark = "rb"
#'     , percent   = TRUE
#'     , base      = 100
#'     , as_dt     = FALSE
#'     )
#' fit
#' plot(x = fit)
#' @export
dmai <- function(.data, method = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE"), logbase = NULL,
                 date = "Date", x = NULL, r = NULL, user_cost = NULL, benchmark = NULL, percent = TRUE, base = 100,
                 as_dt = TRUE) {
  UseMethod("dmai")
}

#' @rdname dmai
#' @export
dmai.default <- function(.data, method = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE"), logbase = NULL,
                         date = "Date", x = NULL, r = NULL, user_cost = NULL, benchmark = NULL, percent = TRUE,
                         base = 100, as_dt = TRUE) {
  fn <- "dmai"
  method  <- .dmai_check_choice(method[1L], .dmai_methods, "method", fn)
  logbase <- if (is.null(logbase)) exp(1) else .dmai_check_number(logbase, "logbase", fn, min = 1 + 1e-9)
  base    <- .dmai_check_number(base, "base", fn, min = .Machine$double.eps)
  as_dt   <- .dmai_check_flag(as_dt, "as_dt", fn)
  inp <- .dmai_input(.data, date, x, r, user_cost, benchmark, percent, fn, need_rates = method == "CE")
  fit <- .dmai_fit(inp, method, logbase, base)
  if (as_dt) {
    out <- data.table::copy(fit$index)
    data.table::setattr(out, "method", method)
    return(out)
  }
  .dmai_result(fit, cls = "index", call = match.call(),
               label = sprintf("%s index of %d components, %d dates", .dmai_method_label(method), inp$n, inp$T))
}

.dmai_method_label <- function(method) {
  c(Barnett = "Divisia (Barnett)", Hancock = "Divisia (Hancock, simple growth)", Fisher = "Fisher ideal",
    SimpleSum = "Simple-sum", CE = "Currency equivalent")[[method]]
}

# the full result of one method
.dmai_fit <- function(inp, method, logbase, base) {
  lk <- .dmai_links(inp, method)
  ss <- sum(inp$X[1L, ])
  lv <- .dmai_levels(lk$link, base, ss, lk$level)
  growth <- .dmai_growth(lk$link, method, logbase)
  index <- data.table::data.table(date = inp$date, DMAI = growth, index = lv$index, value = lv$value)
  data.table::setnames(index, "date", inp$date_name)
  E <- rowSums(inp$U * inp$X)
  price <- data.table::data.table(date = inp$date, expenditure = E, price = E / lv$value)
  price[, price_index := base * price / price[1L]]
  data.table::setnames(price, "date", inp$date_name)
  simple <- rowSums(inp$X)
  simple_sum <- data.table::data.table(date = inp$date, value = simple,
                                       growth = c(NA_real_, 100 * diff(log(simple))))
  data.table::setnames(simple_sum, "date", inp$date_name)
  bench <- data.table::data.table(date = inp$date, benchmark = inp$R * (if (inp$percent) 100 else 1),
                                  envelope = inp$envelope)
  data.table::setnames(bench, "date", inp$date_name)
  list(
      index      = index
    , components = .dmai_components_table(inp, lk$shares)
    , benchmark  = bench
    , price      = price
    , moments    = .dmai_moments_core(inp, lk$shares)
    , simple_sum = simple_sum
    , fisher     = if (method == "Fisher") {
        f <- data.table::data.table(date = inp$date, laspeyres = lk$laspeyres, paasche = lk$paasche, fisher = lk$link)
        data.table::setnames(f, "date", inp$date_name)
        f
      } else NULL
    , settings   = list(method = method, logbase = logbase, base = base, date = inp$date_name, x = inp$x,
                        r = inp$r_names, user_cost = inp$user_cost_names, benchmark = inp$benchmark,
                        percent = inp$percent, periods_per_year = .dmai_periods_per_year(inp$date),
                        n = inp$n, T = inp$T)
    )
}

utils::globalVariables(c("price_index", "price", "."))
