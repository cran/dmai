#-------------------------------------------------------------------------------------------------------------------
# print() and plot() of the results (dispatched by print.dmai_result and plot.dmai_result of common-objects.R)
#-------------------------------------------------------------------------------------------------------------------

.dmai_method_colours <- c(Barnett = "#1f4e9e", Hancock = "#6a3d9a", Fisher = "#2d7d46", SimpleSum = "#b2182b",
                          CE = "#c66b00")

.dmai_x_date <- function(dates) {
  if (inherits(dates, "Date")) ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 10)) else
    .dmai_x_cont(integer = TRUE)
}

.dmai_fmt <- function(v, digits = 4L) format(signif(v, digits), big.mark = ",", trim = TRUE)

.dmai_print_index <- function(x, digits = 4L, ...) {
  s <- x$settings
  idx <- x$index
  cat(attr(x, "label"), "\n", strrep("=", nchar(attr(x, "label"))), "\n", sep = "")
  cat(sprintf("Dates: %s to %s (%d); components: %s\n", format(idx[[1L]][1L]), format(idx[[1L]][nrow(idx)]),
              s$T, paste(s$x, collapse = ", ")))
  cat(sprintf("Benchmark rate: %s\n", if (is.null(s$benchmark)) "the largest component rate at each date" else
    if (is.character(s$benchmark)) sprintf("column %s (upper envelope with the component rates)", s$benchmark) else
      "given values (upper envelope with the component rates)"))
  if (!is.null(s$user_cost)) cat("User costs: given in", paste(s$user_cost, collapse = ", "), "\n")
  g <- idx$DMAI[-1L]
  cat(sprintf("Growth (DMAI, %s): mean %s, sd %s, last %s\n",
              if (s$method == "Hancock") "simple, percent" else if (abs(s$logbase - exp(1)) < 1e-12)
                "log, percent" else sprintf("log base %s x 100", format(s$logbase)),
              .dmai_fmt(mean(g), digits), .dmai_fmt(stats::sd(g), digits), .dmai_fmt(g[length(g)], digits)))
  cat(sprintf("Index: %s at the first date, %s at the last; simple sum: %s\n", .dmai_fmt(idx$index[1L], digits),
              .dmai_fmt(idx$index[nrow(idx)], digits),
              .dmai_fmt(s$base * x$simple_sum$value[nrow(idx)] / x$simple_sum$value[1L], digits)))
  cat("\n")
  print(idx, digits = digits, topn = 5L)
  invisible(x)
}

.dmai_print_user_cost <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n", strrep("=", nchar(attr(x, "label"))), "\n", sep = "")
  d <- x$components
  avg <- d[, .(mean_rate = mean(rate), mean_user_cost = mean(user_cost), mean_share = mean(share),
               last_share = share[.N]), by = component]
  cat("Averages over the dates:\n")
  print(avg, digits = digits)
  n_env <- sum(x$benchmark$envelope)
  if (n_env) cat(sprintf("\nThe benchmark was raised to the largest component rate at %d dates.\n", n_env))
  invisible(x)
}

.dmai_print_moments <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n", strrep("=", nchar(attr(x, "label"))), "\n", sep = "")
  m <- x$moments
  cat(sprintf("Standard deviation of component growth (percent): mean %s, largest %s at %s\n",
              .dmai_fmt(mean(m$sd, na.rm = TRUE), digits), .dmai_fmt(max(m$sd, na.rm = TRUE), digits),
              format(m[[1L]][which.max(m$sd)])))
  cat("\n")
  print(m, digits = digits, topn = 5L)
  invisible(x)
}

.dmai_print_compare <- function(x, digits = 4L, ...) {
  cat(attr(x, "label"), "\n", strrep("=", nchar(attr(x, "label"))), "\n", sep = "")
  print(x$summary, digits = digits)
  invisible(x)
}

# plots ------------------------------------------------------------------------------------------------------------

.dmai_plot_index <- function(x, which = c("index", "growth", "shares", "user_cost", "moments", "price"), ...) {
  which <- .dmai_check_choice(which[1L], c("index", "growth", "shares", "user_cost", "moments", "price"), "which",
                              "plot")
  s <- x$settings
  dn <- s$date
  lab <- .dmai_method_label(s$method)
  if (which %in% c("shares", "user_cost")) return(.dmai_plot_components(x$components, dn, which))
  if (which == "moments") return(.dmai_plot_moments_tab(x$moments, dn))
  if (which == "price") {
    d <- x$price
    return(ggplot2::ggplot(d, ggplot2::aes(x = .data[[dn]], y = .data[["price_index"]])) +
             ggplot2::geom_line(colour = .dmai_palette[1L]) + .dmai_x_date(d[[dn]]) + .dmai_y_cont() +
             ggplot2::labs(x = NULL, y = "dual user-cost price index", title = paste(lab, "aggregate: dual price")) +
             dmai_theme())
  }
  base <- s$base
  d <- data.table::rbindlist(list(
      data.table::data.table(date = x$index[[dn]], method = lab, index = x$index$index, growth = x$index$DMAI)
    , data.table::data.table(date = x$simple_sum[[dn]], method = "Simple sum",
                             index = base * x$simple_sum$value / x$simple_sum$value[1L], growth = x$simple_sum$growth)
    ))
  d[, method := factor(method, levels = c(lab, "Simple sum"))]
  cols <- stats::setNames(c(.dmai_method_colours[[s$method]], .dmai_method_colours[["SimpleSum"]]), c(lab, "Simple sum"))
  if (s$method == "SimpleSum") d <- d[method == lab]
  y <- if (which == "index") "index" else "growth"
  ggplot2::ggplot(d[!is.na(d[[y]])], ggplot2::aes(x = date, y = .data[[y]], colour = method)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    .dmai_x_date(d$date) + .dmai_y_cont() +
    ggplot2::labs(x = NULL, y = if (which == "index") sprintf("index (first date = %s)", format(base)) else
      "growth (percent)", title = if (which == "index") "Monetary aggregate" else "Growth of the monetary aggregate") +
    dmai_theme()
}

.dmai_plot_components <- function(d, dn, which = "shares") {
  y <- if (which == "shares") "share" else "user_cost"
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[dn]], y = .data[[y]], fill = component, colour = component))
  g <- if (which == "shares") g + ggplot2::geom_area(colour = NA, alpha = 0.9) else g + ggplot2::geom_line()
  pal <- grDevices::colorRampPalette(.dmai_palette)(nlevels(d$component))
  g + ggplot2::scale_fill_manual(values = pal, name = NULL) + ggplot2::scale_colour_manual(values = pal, name = NULL) +
    .dmai_x_date(d[[dn]]) + .dmai_y_cont(bars = which == "shares") +
    ggplot2::labs(x = NULL, y = if (which == "shares") "share in the expenditure on monetary services" else "user cost",
                  title = if (which == "shares") "Divisia weights" else "User costs") +
    dmai_theme()
}

.dmai_plot_moments_tab <- function(m, dn) {
  ggplot2::ggplot(m[!is.na(m$sd)], ggplot2::aes(x = .data[[dn]], y = .data[["sd"]])) +
    ggplot2::geom_line(colour = .dmai_palette[1L]) + .dmai_x_date(m[[dn]]) + .dmai_y_cont() +
    ggplot2::labs(x = NULL, y = "standard deviation of component growth (percent)",
                  title = "Divisia second moment") +
    dmai_theme()
}

.dmai_plot_user_cost <- function(x, which = c("shares", "user_cost"), ...) {
  which <- .dmai_check_choice(which[1L], c("shares", "user_cost"), "which", "plot")
  .dmai_plot_components(x$components, x$settings$date, which)
}

.dmai_plot_moments <- function(x, ...) .dmai_plot_moments_tab(x$moments, x$settings$date)

.dmai_plot_compare <- function(x, which = c("index", "growth"), ...) {
  which <- .dmai_check_choice(which[1L], c("index", "growth"), "which", "plot")
  d <- data.table::copy(x$indexes)
  dn <- x$settings$date
  data.table::setnames(d, dn, "date")
  labs <- vapply(levels(d$method), .dmai_method_label, "")
  ggplot2::ggplot(d[!is.na(d[[which]])], ggplot2::aes(x = date, y = .data[[which]], colour = method)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = .dmai_method_colours[levels(d$method)], labels = labs, name = NULL) +
    .dmai_x_date(d$date) + .dmai_y_cont() +
    ggplot2::labs(x = NULL, y = if (which == "index") sprintf("index (first date = %s)", format(x$settings$base)) else
      "growth (percent)", title = "Monetary aggregates") +
    dmai_theme()
}

#' Plot a dmai result with ggplot2
#'
#' @description \code{autoplot()} methods for the results of \pkg{dmai}; \code{plot()} draws the same figures.
#' @param object a result of \code{\link{dmai}(..., as_dt = FALSE)}, \code{\link{dmai_compare}()},
#'   \code{\link{dmai_user_cost}()} or \code{\link{dmai_moments}()}.
#' @param which the figure: for \code{dmai()} results \code{"index"} (default, with the simple sum),
#'   \code{"growth"}, \code{"shares"}, \code{"user_cost"}, \code{"moments"} or \code{"price"}; for
#'   \code{dmai_compare()} \code{"index"} or \code{"growth"}; for \code{dmai_user_cost()} \code{"shares"} or
#'   \code{"user_cost"}.
#' @param ... not used.
#' @return A \code{ggplot} object.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' data(money_sim)
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
#' ggplot2::autoplot(
#'     object = fit
#'   , which  = "shares"
#'   )
#' @rawNamespace S3method(ggplot2::autoplot, dmai_result)
autoplot.dmai_result <- function(object, which = NULL, ...) {
  if (is.null(which)) plot(object, ...) else plot(object, which = which, ...)
}

utils::globalVariables(c("component", "rate", "user_cost", "share", "method", "date"))
