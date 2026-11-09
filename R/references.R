#-------------------------------------------------------------------------------------------------------------------
# The sources of every method: one registry gives the short citation printed under each result, the references of
# the interpretations and the application, dmai_cite(), inst/CITATION and inst/REFERENCES.bib (written from it by
# tools/write-citation.R).
#-------------------------------------------------------------------------------------------------------------------

.dmai_ref <- function(key, short, text, ...) {
  list(key = key, short = short, text = text, bib = utils::bibentry(key = key, ...))
}

.dmai_p <- function(given, family) utils::person(given = given, family = family)

.dmai_refs_list <- function() {
  p <- .dmai_p
  refs <- list(
    .dmai_ref("Barnett1978", "Barnett (1978)",
              paste("Barnett, W. A. (1978). The user cost of money. Economics Letters, 1(2), 145-149.",
                    "doi:10.1016/0165-1765(78)90051-4"),
              bibtype = "Article", title = "The User Cost of Money", author = p(c("William", "A."), "Barnett"),
              journal = "Economics Letters", year = "1978", volume = "1", number = "2", pages = "145--149",
              doi = "10.1016/0165-1765(78)90051-4"),
    .dmai_ref("Barnett1980", "Barnett (1980)",
              paste("Barnett, W. A. (1980). Economic monetary aggregates: an application of index number and",
                    "aggregation theory. Journal of Econometrics, 14(1), 11-48. doi:10.1016/0304-4076(80)90070-6"),
              bibtype = "Article",
              title = "Economic Monetary Aggregates: An Application of Index Number and Aggregation Theory",
              author = p(c("William", "A."), "Barnett"), journal = "Journal of Econometrics", year = "1980",
              volume = "14", number = "1", pages = "11--48", doi = "10.1016/0304-4076(80)90070-6"),
    .dmai_ref("Hancock1985", "Hancock (1985)",
              paste("Hancock, D. (1985). The financial firm: production with monetary and nonmonetary goods.",
                    "Journal of Political Economy, 93(5), 859-880. doi:10.1086/261339"),
              bibtype = "Article", title = "The Financial Firm: Production with Monetary and Nonmonetary Goods",
              author = p("Diana", "Hancock"), journal = "Journal of Political Economy", year = "1985",
              volume = "93", number = "5", pages = "859--880", doi = "10.1086/261339"),
    .dmai_ref("Tornqvist1936", "Tornqvist (1936)",
              paste("Tornqvist, L. (1936). The Bank of Finland's consumption price index. Bank of Finland Monthly",
                    "Bulletin, 10, 1-8."),
              bibtype = "Article", title = "The {Bank of Finland's} Consumption Price Index",
              author = p("Leo", "T{\\\"o}rnqvist"), journal = "Bank of Finland Monthly Bulletin", year = "1936",
              volume = "10", pages = "1--8"),
    .dmai_ref("Theil1967", "Theil (1967)",
              "Theil, H. (1967). Economics and Information Theory. North-Holland, Amsterdam.",
              bibtype = "Book", title = "Economics and Information Theory", author = p("Henri", "Theil"),
              publisher = "North-Holland", address = "Amsterdam", year = "1967"),
    .dmai_ref("Fisher1922", "Fisher (1922)",
              "Fisher, I. (1922). The Making of Index Numbers. Houghton Mifflin, Boston.",
              bibtype = "Book", title = "The Making of Index Numbers", author = p("Irving", "Fisher"),
              publisher = "Houghton Mifflin", address = "Boston", year = "1922"),
    .dmai_ref("Diewert1976", "Diewert (1976)",
              paste("Diewert, W. E. (1976). Exact and superlative index numbers. Journal of Econometrics, 4(2),",
                    "115-145. doi:10.1016/0304-4076(76)90009-9"),
              bibtype = "Article", title = "Exact and Superlative Index Numbers", author = p(c("W.", "Erwin"), "Diewert"),
              journal = "Journal of Econometrics", year = "1976", volume = "4", number = "2", pages = "115--145",
              doi = "10.1016/0304-4076(76)90009-9"),
    .dmai_ref("Rotemberg1995", "Rotemberg et al. (1995)",
              paste("Rotemberg, J. J., Driscoll, J. C. and Poterba, J. M. (1995). Money, output, and prices:",
                    "evidence from a new monetary aggregate. Journal of Business and Economic Statistics, 13(1),",
                    "67-83."),
              bibtype = "Article", title = "Money, Output, and Prices: Evidence from a New Monetary Aggregate",
              author = c(p(c("Julio", "J."), "Rotemberg"), p(c("John", "C."), "Driscoll"),
                         p(c("James", "M."), "Poterba")),
              journal = "Journal of Business and Economic Statistics", year = "1995", volume = "13", number = "1",
              pages = "67--83"),
    .dmai_ref("BarnettSerletis1990", "Barnett and Serletis (1990)",
              paste("Barnett, W. A. and Serletis, A. (1990). A dispersion-dependency diagnostic test for aggregation",
                    "error: with applications to monetary economics and income distribution. Journal of",
                    "Econometrics, 43(1-2), 5-34. doi:10.1016/0304-4076(90)90104-2"),
              bibtype = "Article",
              title = paste("A Dispersion-Dependency Diagnostic Test for Aggregation Error: With Applications to",
                            "Monetary Economics and Income Distribution"),
              author = c(p(c("William", "A."), "Barnett"), p("Apostolos", "Serletis")),
              journal = "Journal of Econometrics", year = "1990", volume = "43", number = "1--2", pages = "5--34",
              doi = "10.1016/0304-4076(90)90104-2"),
    .dmai_ref("CPIManual2004", "ILO et al. (2004)",
              paste("ILO, IMF, OECD, UNECE, Eurostat and The World Bank (2004). Consumer Price Index Manual: Theory",
                    "and Practice. International Labour Office, Geneva. Chapter 19: Price indices using an",
                    "artificial data set."),
              bibtype = "Book", title = "Consumer Price Index Manual: Theory and Practice",
              author = utils::person("{ILO, IMF, OECD, UNECE, Eurostat and The World Bank}"),
              publisher = "International Labour Office", address = "Geneva", year = "2004")
  )
  stats::setNames(refs, vapply(refs, `[[`, "", "key"))
}

# the sources of a result
.dmai_sources_of <- function(x) {
  cls <- .dmai_cls(x)
  m <- x$settings$method
  switch(cls,
    index     = c("Barnett1978", "Barnett1980",
                  switch(m, Barnett = c("Tornqvist1936", "Theil1967"), Hancock = "Hancock1985",
                         Fisher = c("Fisher1922", "Diewert1976"), CE = "Rotemberg1995", character(0))),
    user_cost = "Barnett1978",
    moments   = c("Theil1967", "BarnettSerletis1990"),
    compare   = c("Barnett1978", "Barnett1980", "Hancock1985", "Fisher1922", "Diewert1976", "Rotemberg1995"),
    character(0))
}

# "Sources: Barnett (1978); ..." for the end of a print
.dmai_sources_line <- function(x) {
  k <- unique(.dmai_sources_of(x))
  if (!length(k)) return(invisible(NULL))
  r <- .dmai_refs_list()[k]
  txt <- strwrap(paste0("Sources: ", paste(vapply(r, `[[`, "", "short"), collapse = "; "),
                        ". Full references: dmai_cite(x)."), width = getOption("width", 80L))
  cat("\n", paste(txt, collapse = "\n"), "\n", sep = "")
  invisible(NULL)
}

#' References of a result or of the package
#'
#' @description
#' \code{dmai_cite()} gives the references of the methods behind a result of the package, or the references of every
#' method in the package, as text or BibTeX. \code{citation("dmai")} cites the package and its main methods.
#'
#' @param x a result of a function of the package (with \code{as_dt = FALSE} for \code{\link{dmai}()}), or
#'   \code{NULL} (default) for every reference of the package.
#' @param style \code{"text"} (default) or \code{"bibtex"}.
#' @return An object of class \code{bibentry} (printed in the chosen style).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @examples
#' dmai_cite(
#'     x     = NULL
#'   , style = "text"
#'   )
#' data(money_sim)
#' fit <-
#'   dmai(
#'       .data     = money_sim
#'     , method    = "Fisher"
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
#' dmai_cite(
#'     x     = fit
#'   , style = "bibtex"
#'   )
#' @export
dmai_cite <- function(x = NULL, style = c("text", "bibtex")) UseMethod("dmai_cite")

#' @rdname dmai_cite
#' @export
dmai_cite.default <- function(x = NULL, style = c("text", "bibtex")) {
  style <- .dmai_check_choice(style[1L], c("text", "bibtex"), "style", "dmai_cite")
  refs <- .dmai_refs_list()
  keys <- if (is.null(x)) names(refs) else {
    if (!inherits(x, "dmai_result")) .dmai_stop("dmai_cite", "`x` must be a result of the package (as_dt = FALSE) or NULL.")
    unique(.dmai_sources_of(x))
  }
  out <- do.call(c, lapply(refs[keys], `[[`, "bib"))
  structure(out, dmai_style = style, class = c("dmai_cite", class(out)))
}

#' @export
print.dmai_cite <- function(x, ...) {
  style <- attr(x, "dmai_style")
  y <- x
  class(y) <- setdiff(class(y), "dmai_cite")
  if (!length(y)) {
    cat("No references.\n")
    return(invisible(x))
  }
  if (identical(style, "bibtex")) print(y, style = "Bibtex") else {
    refs <- .dmai_refs_list()
    keys <- vapply(unclass(y), function(e) attr(e, "key"), "")
    cat(paste(vapply(refs[keys], `[[`, "", "text"), collapse = "\n\n"), "\n")
  }
  invisible(x)
}
