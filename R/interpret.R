#' Plain-language interpretation of a result
#'
#' @description
#' \code{dmai_interpret()} turns a result of the package into a short report: a headline, findings, recommendations
#' and cautions, computed from its numbers, with the sources of the method. It covers \code{\link{dmai}()} (with
#' \code{as_dt = FALSE}), \code{\link{dmai_compare}()}, \code{\link{dmai_user_cost}()} and
#' \code{\link{dmai_moments}()}.
#'
#' @details
#' Growth per year is the average growth between the first and last dates, annualised with the spacing of the
#' dates (monthly, quarterly, half-yearly or annual) when the date column is of class \code{Date}; otherwise growth
#' is reported per period.
#'
#' @param x a result of a function of the package.
#' @param top number of dates or components named in the findings (default 3).
#' @param ... unused.
#' @param style \code{"text"} (default) or \code{"markdown"}.
#' @param width line width of the text (default \code{getOption("width")}).
#' @return An object of class \code{"dmai_interpretation"}: a list with \code{analysis}, \code{title},
#'   \code{headline}, \code{findings}, \code{recommendations}, \code{cautions}, \code{facts} (the numbers behind the
#'   sentences), \code{guide} (how to read the method) and \code{sources}. \code{format()} gives it as text or
#'   Markdown.
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{dmai_cite}}, \code{\link{dmai_app}}
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
#' dmai_interpret(
#'     x   = fit
#'   , top = 3
#'   )
#' @export
dmai_interpret <- function(x, top = 3L, ...) UseMethod("dmai_interpret")

#' @rdname dmai_interpret
#' @export
dmai_interpret.default <- function(x, top = 3L, ...) {
  fn  <- "dmai_interpret"
  top <- .dmai_check_number(top, "top", fn, min = 1, integer = TRUE)
  if (!inherits(x, "dmai_result")) {
    .dmai_stop(fn, "`x` must be a result of a function of the dmai package (for dmai(), use as_dt = FALSE).")
  }
  cls <- .dmai_cls(x)
  f <- get0(paste0(".dmai_int_", cls), envir = asNamespace("dmai"), inherits = FALSE)
  if (is.null(f)) .dmai_stop(fn, "no interpretation is defined for ", cls, " results.")
  out <- f(x, list(top = top))
  out$analysis <- cls
  out$guide    <- .dmai_guides[[cls]]
  out$sources  <- unname(vapply(.dmai_refs_list()[unique(.dmai_sources_of(x))], `[[`, "", "text"))
  structure(out[c("analysis", "title", "headline", "findings", "recommendations", "cautions", "facts", "guide",
                  "sources")], class = "dmai_interpretation")
}

#' @rdname dmai_interpret
#' @export
print.dmai_interpretation <- function(x, ...) {
  cat(format(x, style = "text"), sep = "\n")
  invisible(x)
}

#' @rdname dmai_interpret
#' @export
format.dmai_interpretation <- function(x, style = c("text", "markdown"), width = getOption("width", 80L), ...) {
  style <- .dmai_check_choice(style[1L], c("text", "markdown"), "style", "format")
  wrap <- function(s, prefix) {
    if (!length(s)) return(character(0))
    unlist(lapply(s, function(z) {
      w <- strwrap(z, width = max(40L, width - 2L) - nchar(prefix))
      c(paste0(prefix, w[1L]), if (length(w) > 1L) paste0(strrep(" ", nchar(prefix)), w[-1L]))
    }))
  }
  sec <- function(h, s) if (length(s)) c("", if (style == "markdown") paste("###", h) else paste0(h, ":"),
                                          wrap(s, "- ")) else character(0)
  c(if (style == "markdown") paste("##", x$title) else x$title,
    "", wrap(x$headline, ""),
    sec("Findings", x$findings), sec("Recommendations", x$recommendations), sec("Cautions", x$cautions),
    sec("Sources", x$sources))
}

# helpers ----------------------------------------------------------------------------------------------------------

.dmai_pct <- function(v, d = 1L) formatC(v, format = "f", digits = d)

# average growth per year (percent) between the first and last values, or per period when the frequency is unknown
.dmai_avg_growth <- function(first, last, n_periods, ppy) {
  per <- if (is.na(ppy)) 1 else ppy
  list(value = 100 * ((last / first)^(per / n_periods) - 1), unit = if (is.na(ppy)) "per period" else "a year")
}

.dmai_date_txt <- function(d) format(d)

# interpretations --------------------------------------------------------------------------------------------------

.dmai_int_index <- function(x, o) {
  s <- x$settings
  idx <- x$index
  dn <- s$date
  T <- nrow(idx)
  lab <- .dmai_method_label(s$method)
  g  <- .dmai_avg_growth(idx$index[1L], idx$index[T], T - 1L, s$periods_per_year)
  gs <- .dmai_avg_growth(x$simple_sum$value[1L], x$simple_sum$value[T], T - 1L, s$periods_per_year)
  gap <- g$value - gs$value
  headline <- sprintf("From %s to %s the %s aggregate grew %s%% %s, against %s%% for the simple sum (%s).",
                      .dmai_date_txt(idx[[dn]][1L]), .dmai_date_txt(idx[[dn]][T]), lab, .dmai_pct(g$value), g$unit,
                      .dmai_pct(gs$value), if (abs(gap) < 0.05) "about the same" else
                        sprintf("%s points %s", .dmai_pct(abs(gap)), if (gap < 0) "less" else "more"))
  gr <- idx$DMAI
  ord <- order(gr, decreasing = TRUE, na.last = NA)
  hi <- utils::head(ord, o$top)
  lo <- utils::head(rev(ord), o$top)
  comp <- x$components
  sh_first <- comp[comp[[dn]] == idx[[dn]][1L], .(component, share)]
  sh_last  <- comp[comp[[dn]] == idx[[dn]][T], .(component, share)]
  ch <- merge(sh_first, sh_last, by = "component", suffixes = c("_first", "_last"))
  ch[, change := share_last - share_first]
  ch <- ch[order(-abs(change))]
  top_w <- sh_last[order(-share)][seq_len(min(o$top, nrow(sh_last)))]
  findings <- c(
      sprintf("Largest growth at %s (%s); smallest at %s (%s)%s.",
              paste(.dmai_date_txt(idx[[dn]][hi]), collapse = ", "), paste(.dmai_pct(gr[hi], 2L), collapse = ", "),
              paste(.dmai_date_txt(idx[[dn]][lo]), collapse = ", "), paste(.dmai_pct(gr[lo], 2L), collapse = ", "),
              if (s$method == "Hancock") ", simple growth in percent" else "")
    , sprintf("At the last date the largest weights are %s.",
              paste(sprintf("%s (%s%%)", top_w$component, .dmai_pct(100 * top_w$share)), collapse = ", "))
    , sprintf("The weights moved most for %s.",
              paste(sprintf("%s (%s to %s%%)", ch$component[seq_len(min(o$top, nrow(ch)))],
                            .dmai_pct(100 * ch$share_first[seq_len(min(o$top, nrow(ch)))]),
                            .dmai_pct(100 * ch$share_last[seq_len(min(o$top, nrow(ch)))])), collapse = ", "))
    , if (abs(gap) >= 0.5) sprintf(paste("The simple sum %s money growth: it gives the same weight to interest-bearing",
                                         "deposits, which are held largely as savings, as to currency and demand",
                                         "deposits."), if (gap < 0) "overstates" else "understates")
    )
  zero <- comp[, .(zero = mean(user_cost <= 0)), by = component][zero > 0.5]
  cautions <- c(
      if (is.null(s$benchmark) && is.null(s$user_cost))
        paste("The benchmark is the largest component rate, so that component has a user cost of zero and no weight;",
              "a benchmark above every component rate (a bond yield, for example) is the usual choice: give it with",
              "`benchmark`.")
    , if (nrow(zero)) sprintf("Zero user cost (no weight) at most dates for %s.", paste(zero$component, collapse = ", "))
    , if (sum(x$benchmark$envelope)) sprintf("The benchmark was below a component rate at %d dates and was raised to it.",
                                             sum(x$benchmark$envelope))
    , if (s$method != "Hancock" && abs(s$logbase - exp(1)) > 1e-12)
        sprintf("DMAI uses logarithms to base %s, so it is not a growth rate in percent (version 0.5.0 used base 10).",
                format(s$logbase))
    , if (T < 8L) "Few dates: the averages are rough."
    )
  list(title = paste(lab, "monetary aggregate"), headline = headline, findings = findings,
       recommendations = c("Compare the methods with dmai_compare(); check how the result depends on the benchmark rate.",
                           "dmai_moments() shows when the components grow at very different rates."),
       cautions = cautions,
       facts = list(growth = g$value, simple_growth = gs$value, gap = gap, shares_last = top_w))
}

.dmai_int_compare <- function(x, o) {
  sm <- x$summary
  unit <- if (is.na(x$settings$periods_per_year)) "per period" else "a year"
  v <- if (is.na(x$settings$periods_per_year)) sm$mean_growth else sm$annual_growth
  labs <- vapply(as.character(sm$method), .dmai_method_label, "")
  o2 <- order(v)
  headline <- sprintf("Average growth %s ranges from %s%% (%s) to %s%% (%s).", unit, .dmai_pct(v[o2[1L]]),
                      labs[o2[1L]], .dmai_pct(v[o2[length(o2)]]), labs[o2[length(o2)]])
  findings <- sprintf("%s: %s%% %s, standard deviation of growth %s.", labs, .dmai_pct(v), unit,
                      .dmai_pct(sm$sd_growth, 2L))
  cautions <- if (all(c("Barnett", "Fisher") %in% sm$method)) {
    d <- abs(v[sm$method == "Barnett"] - v[sm$method == "Fisher"])
    if (d > 0.1) sprintf("The Divisia and Fisher indexes differ by %s points; both are superlative and usually agree closely.",
                         .dmai_pct(d, 2L))
  }
  list(title = "Monetary aggregates compared", headline = headline, findings = findings,
       recommendations = "Prefer a superlative index (Divisia or Fisher) for policy analysis; report the simple sum for reference.",
       cautions = cautions, facts = list(summary = sm))
}

.dmai_int_user_cost <- function(x, o) {
  d <- x$components
  avg <- d[, .(share = mean(share), user_cost = mean(user_cost)), by = component][order(-share)]
  top <- avg[seq_len(min(o$top, nrow(avg)))]
  headline <- sprintf("On average %s carry the largest weights in the expenditure on monetary services.",
                      paste(sprintf("%s (%s%%)", top$component, .dmai_pct(100 * top$share)), collapse = ", "))
  low <- avg[share < 0.01]
  list(title = "User costs and weights", headline = headline,
       findings = c(sprintf("Average user cost per unit: %s.", paste(sprintf("%s %s", avg$component,
                                                                               signif(avg$user_cost, 3)), collapse = "; ")),
                    if (nrow(low)) sprintf("Weights below 1%% on average: %s.", paste(low$component, collapse = ", "))),
       recommendations = "Use these weights with dmai() to build the Divisia aggregate.",
       cautions = if (sum(x$benchmark$envelope)) sprintf("The benchmark was raised to a component rate at %d dates.",
                                                       sum(x$benchmark$envelope)),
       facts = list(averages = avg))
}

.dmai_int_moments <- function(x, o) {
  m <- x$moments
  dn <- x$settings$date
  ok <- !is.na(m$sd)
  ord <- order(m$sd, decreasing = TRUE, na.last = NA)
  hi <- utils::head(ord, o$top)
  headline <- sprintf("The growth rates of the components differ by %s percentage points on average (Divisia standard deviation).",
                      .dmai_pct(mean(m$sd[ok]), 2L))
  list(title = "Divisia second moments", headline = headline,
       findings = sprintf("The largest dispersion is at %s (%s).", paste(.dmai_date_txt(m[[dn]][hi]), collapse = ", "),
                          paste(.dmai_pct(m$sd[hi], 2L), collapse = ", ")),
       recommendations = "Large dispersion is when the simple sum and the Divisia index part ways; compare them at those dates.",
       cautions = NULL, facts = list(mean_sd = mean(m$sd[ok])))
}

.dmai_guides <- list(
    index = paste("Each component is weighted by its share in the expenditure on monetary services. The price of the",
                  "services of an asset, its user cost, is the interest given up by holding it instead of the",
                  "benchmark asset: assets paying close to the benchmark get small weights, currency and demand",
                  "deposits large ones.")
  , compare = paste("The Divisia (Barnett) and Fisher indexes weight components by their user costs; the Hancock",
                    "variant uses simple growth rates; the simple sum weights all components equally; the currency",
                    "equivalent index counts each component by the share of its yield that is given up.")
  , user_cost = paste("The user cost of an asset is (R - r) / (1 + R): the benchmark rate R minus its own rate r,",
                      "discounted one period.")
  , moments = paste("The Divisia variance measures how much the growth rates of the components differ at each date,",
                    "weighted by their shares.")
  )

utils::globalVariables(c("change", "share_last", "share_first", "zero"))
