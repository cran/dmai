#-------------------------------------------------------------------------------------------------------------------
# User costs and shares, Divisia second moments, and the comparison of the methods
#-------------------------------------------------------------------------------------------------------------------

# Divisia second moments (Theil 1967; Barnett and Serletis 1990) from the average shares
.dmai_moments_core <- function(inp, sh = .dmai_shares(inp)) {
  dl <- rbind(NA_real_, diff(log(inp$X)))
  w  <- sh$Sbar
  kappa <- rowSums(w * dl)
  K <- rowSums(w * (dl - kappa)^2)
  # price moments over the components with a positive user cost at both dates, weights renormalised
  U <- inp$U
  ok <- rbind(FALSE, U[-1L, , drop = FALSE] > 0 & U[-inp$T, , drop = FALSE] > 0)
  lu <- matrix(NA_real_, inp$T, inp$n)
  lu[-1L, ] <- log(pmax(U[-1L, , drop = FALSE], .Machine$double.xmin)) - log(pmax(U[-inp$T, , drop = FALSE],
                                                                                  .Machine$double.xmin))
  wp <- ifelse(ok, w, 0)
  wp <- wp / rowSums(wp)
  lu[!ok] <- 0
  pi_mean <- rowSums(wp * lu)
  Kp <- rowSums(wp * (lu - pi_mean)^2)
  m <- data.table::data.table(date = inp$date, growth = 100 * kappa, variance = 1e4 * K, sd = 100 * sqrt(K),
                              price_growth = 100 * pi_mean, price_variance = 1e4 * Kp)
  m[1L, (names(m)[-1L]) := NA_real_]
  data.table::setnames(m, "date", inp$date_name)
  m
}

#' User costs and expenditure shares of monetary assets
#'
#' @description
#' \code{dmai_user_cost()} gives, for every date and component, the rate, the user cost
#' \eqn{\pi_{it} = (R_t - r_{it})/(1 + R_t)}{pi_it = (R_t - r_it) / (1 + R_t)} (Barnett 1978) and the share of the
#' component in the expenditure on monetary services, the weights of the Divisia index.
#'
#' @inheritParams dmai
#' @return An object of class \code{c("dmai_user_cost", "dmai_result", "list")}: \code{components} (date,
#'   component, quantity, rate, user cost, share), \code{benchmark} (the benchmark rate and whether the upper
#'   envelope raised it) and \code{settings}. \code{plot()} draws the shares over time.
#' @references
#' Barnett, W. A. (1978). The user cost of money. \emph{Economics Letters}, \strong{1}(2), 145--149.
#' \doi{10.1016/0165-1765(78)90051-4}
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @family dmai analyses
#' @examples
#' data(money_sim)
#' uc <-
#'   dmai_user_cost(
#'       .data     = money_sim
#'     , date      = "Date"
#'     , x         = NULL
#'     , r         = NULL
#'     , benchmark = "rb"
#'     , percent   = TRUE
#'     )
#' uc
#' plot(x = uc)
#' @export
dmai_user_cost <- function(.data, date = "Date", x = NULL, r = NULL, benchmark = NULL, percent = TRUE) {
  UseMethod("dmai_user_cost")
}

#' @rdname dmai_user_cost
#' @export
dmai_user_cost.default <- function(.data, date = "Date", x = NULL, r = NULL, benchmark = NULL, percent = TRUE) {
  fn  <- "dmai_user_cost"
  inp <- .dmai_input(.data, date, x, r, NULL, benchmark, percent, fn)
  bench <- data.table::data.table(date = inp$date, benchmark = inp$R * (if (percent) 100 else 1),
                                  envelope = inp$envelope)
  data.table::setnames(bench, "date", inp$date_name)
  .dmai_result(list(components = .dmai_components_table(inp), benchmark = bench,
                    settings = list(date = inp$date_name, x = inp$x, r = inp$r_names, benchmark = inp$benchmark,
                                    percent = percent, n = inp$n, T = inp$T)),
               cls = "user_cost", call = match.call(),
               label = sprintf("User costs of %d components, %d dates", inp$n, inp$T))
}

#' Divisia second moments
#'
#' @description
#' \code{dmai_moments()} gives the Divisia mean and variance of the growth rates of the components at every date
#' (Theil 1967; Barnett and Serletis 1990): \deqn{\kappa_t = \sum_i \bar{s}_{it}\,\Delta\log x_{it}, \qquad K_t =
#' \sum_i \bar{s}_{it}\left(\Delta\log x_{it} - \kappa_t\right)^2,}{kappa_t = sum_i sbar_it dlog x_it, K_t = sum_i
#' sbar_it (dlog x_it - kappa_t)^2,} and the same for the user costs. A large variance means the components grow at
#' very different rates, so that the choice of weights matters and the simple sum is a poor guide; Barnett and
#' Serletis use it as a diagnostic of aggregation error.
#'
#' @inheritParams dmai
#' @param .data a data frame as in \code{\link{dmai}()}, or a result of \code{dmai(..., as_dt = FALSE)}.
#' @param ... further arguments of \code{\link{dmai}()} (\code{date}, \code{x}, \code{r}, \code{user_cost},
#'   \code{benchmark}, \code{percent}) when \code{.data} is a data frame.
#' @return An object of class \code{c("dmai_moments", "dmai_result", "list")}: \code{moments} (a \code{data.table}
#'   with the date, \code{growth} = \eqn{100\kappa_t}{100 kappa_t}, \code{variance} = \eqn{10^4K_t}{10^4 K_t},
#'   \code{sd}, \code{price_growth} and \code{price_variance}) and \code{settings}. \code{plot()} draws the
#'   standard deviation over time.
#' @references
#' Barnett, W. A. and Serletis, A. (1990). A dispersion-dependency diagnostic test for aggregation error: with
#' applications to monetary economics and income distribution. \emph{Journal of Econometrics}, \strong{43}(1--2),
#' 5--34. \doi{10.1016/0304-4076(90)90104-2}
#'
#' Theil, H. (1967). \emph{Economics and Information Theory}. North-Holland, Amsterdam.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @family dmai analyses
#' @examples
#' data(money_sim)
#' m <-
#'   dmai_moments(
#'       .data     = money_sim
#'     , benchmark = "rb"
#'     )
#' m
#' plot(x = m)
#' @export
dmai_moments <- function(.data, ...) UseMethod("dmai_moments")

#' @rdname dmai_moments
#' @export
dmai_moments.default <- function(.data, ..., date = "Date", x = NULL, r = NULL, user_cost = NULL, benchmark = NULL,
                                 percent = TRUE) {
  fn  <- "dmai_moments"
  inp <- .dmai_input(.data, date, x, r, user_cost, benchmark, percent, fn)
  .dmai_result(list(moments = .dmai_moments_core(inp),
                    settings = list(date = inp$date_name, x = inp$x, n = inp$n, T = inp$T,
                                    periods_per_year = .dmai_periods_per_year(inp$date))),
               cls = "moments", call = match.call(),
               label = sprintf("Divisia second moments of %d components, %d dates", inp$n, inp$T))
}

#' @rdname dmai_moments
#' @export
dmai_moments.dmai_index <- function(.data, ...) {
  .dmai_result(list(moments = data.table::copy(.data$moments), settings = .data$settings),
               cls = "moments", call = match.call(),
               label = sprintf("Divisia second moments of %d components, %d dates", .data$settings$n,
                               .data$settings$T))
}

#' Monetary aggregates by every method
#'
#' @description
#' \code{dmai_compare()} computes the Divisia (Barnett and Hancock), Fisher ideal, simple-sum and currency
#' equivalent aggregates of the same data, as indexes equal to \code{base} at the first date, with their growth
#' rates and average annual growth.
#'
#' @inheritParams dmai
#' @param methods the methods to compare (default: all five; \code{"CE"} is dropped when no rates are given).
#' @return An object of class \code{c("dmai_compare", "dmai_result", "list")}: \code{indexes} (a long
#'   \code{data.table}: date, method, index, growth), \code{summary} (by method: average growth per period and per
#'   year, standard deviation of growth, last index value) and \code{settings}. \code{plot()} draws the indexes or
#'   their growth (\code{which = "growth"}).
#' @references
#' Barnett, W. A. (1980). Economic monetary aggregates: an application of index number and aggregation theory.
#' \emph{Journal of Econometrics}, \strong{14}(1), 11--48. \doi{10.1016/0304-4076(80)90070-6}
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @family dmai analyses
#' @examples
#' data(money_sim)
#' cmp <-
#'   dmai_compare(
#'       .data     = money_sim
#'     , methods   = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE")
#'     , logbase   = NULL
#'     , date      = "Date"
#'     , x         = NULL
#'     , r         = NULL
#'     , user_cost = NULL
#'     , benchmark = "rb"
#'     , percent   = TRUE
#'     , base      = 100
#'     )
#' cmp
#' plot(x = cmp)
#' @export
dmai_compare <- function(.data, methods = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE"), logbase = NULL,
                         date = "Date", x = NULL, r = NULL, user_cost = NULL, benchmark = NULL, percent = TRUE,
                         base = 100) {
  UseMethod("dmai_compare")
}

#' @rdname dmai_compare
#' @export
dmai_compare.default <- function(.data, methods = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE"),
                                 logbase = NULL, date = "Date", x = NULL, r = NULL, user_cost = NULL,
                                 benchmark = NULL, percent = TRUE, base = 100) {
  fn <- "dmai_compare"
  if (!is.character(methods) || !length(methods) || !all(methods %in% .dmai_methods)) {
    .dmai_stop(fn, "`methods` must be some of ", paste0("\"", .dmai_methods, "\"", collapse = ", "), ".")
  }
  methods <- unique(methods)
  logbase <- if (is.null(logbase)) exp(1) else .dmai_check_number(logbase, "logbase", fn, min = 1 + 1e-9)
  base <- .dmai_check_number(base, "base", fn, min = .Machine$double.eps)
  inp <- .dmai_input(.data, date, x, r, user_cost, benchmark, percent, fn)
  if ("CE" %in% methods && is.null(inp$r)) {
    .dmai_warn(fn, "no rates given: the currency equivalent index is left out.")
    methods <- setdiff(methods, "CE")
  }
  ppy <- .dmai_periods_per_year(inp$date)
  long <- data.table::rbindlist(lapply(methods, function(m) {
    f <- .dmai_fit(inp, m, logbase, base)$index
    data.table::data.table(date = inp$date, method = m, index = f$index, growth = f$DMAI)
  }))
  long[, method := factor(method, levels = methods)]
  summ <- long[, .(mean_growth = mean(growth, na.rm = TRUE),
                   annual_growth = if (is.na(ppy)) NA_real_ else
                     100 * ((index[.N] / index[1L])^(ppy / (.N - 1L)) - 1),
                   sd_growth = stats::sd(growth, na.rm = TRUE), last_index = index[.N]), by = method]
  data.table::setnames(long, "date", inp$date_name)
  .dmai_result(list(indexes = long, summary = summ,
                    settings = list(methods = methods, logbase = logbase, base = base, date = inp$date_name,
                                    x = inp$x, n = inp$n, T = inp$T, periods_per_year = ppy)),
               cls = "compare", call = match.call(),
               label = sprintf("%d monetary aggregates of %d components, %d dates", length(methods), inp$n, inp$T))
}

utils::globalVariables(c("method", "growth", "index", "mean_growth", "sd_growth", ".N"))
