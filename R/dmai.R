#' @name    dmai
#' @aliases dmai
#' @title Divisia Monetary Aggregates Index
#' @description Calculates Divisia monetary aggregates index as given in Barnett, W. A. (1980).
#'
#' @param .data   data.frame
#' @param method  Method to calculate Divisia monetary aggregates index, Barnett or Hancock
#' @param logbase base of the log to be used i.e 10 for log to the base 10 and exp(1) for natural log to be used
#'
#'
#'
#' @return Divisia Monetary Aggregates Index
#'
#' @author
#' \enumerate{
#'          \item Muhammad Yaseen (\email{myaseen208@@gmail.com})
#'          \item Ahmad Nadeem (\email{Ahmed.Nadeem@@sbp.org.pk})
#'          }
#'
#' @references
#' Barnett, W. A. (1980).
#'  Economic Monetary Aggregates: An Application of Aggregation and Index Number Theory.
#'  \emph{Journal of Econometrics}.
#'  \strong{14}(1):11-48. (\href{https://www.sciencedirect.com/science/article/pii/0304407680900706}{https://www.sciencedirect.com/science/article/pii/0304407680900706})
#'
#' @importFrom dplyr arrange distinct filter group_by lag mutate select ungroup
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr extract gather spread
#' @import  ggplot2
#' @import stringr
#'
#' @export
#'
#' @examples
#'
#' Data <-
#'   tibble::tibble(
#'     Date = paste(c("Jun", "Dec"), rep(seq(from = 2000, to = 2017, by = 1), each = 2), sep = "-")
#'   , x1    = runif(n = 36, min = 162324, max = 2880189)
#'   , x2    = runif(n = 36, min = 2116,   max =   14542)
#'   , x3    = runif(n = 36, min = 92989,  max = 3019556)
#'   , x4    = runif(n = 36, min = 205155, max = 4088784)
#'   , x5    = runif(n = 36, min = 6082,   max =  186686)
#'   , x6    = runif(n = 36, min = 11501,  max =   50677)
#'   , x7    = runif(n = 36, min = 61888,  max =  901419)
#'   , x8    = runif(n = 36, min = 13394,  max =  347020)
#'   , x9    = runif(n = 36, min = 25722,  max =  701887)
#'   , x10   = runif(n = 36, min = 6414,   max =   37859)
#'   , x11   = runif(n = 36, min = 11688,  max =  113865)
#'   , x12   = runif(n = 36, min = 2311,   max =   23130)
#'   , x13   = runif(n = 36, min = 23955,  max =  161318)
#'   , r1    = runif(n = 36, min = 0.00,   max =  0.00)
#'   , r2    = runif(n = 36, min = 0.00,   max = 0.00)
#'   , r3    = runif(n = 36, min = 0.00,   max = 0.00)
#'   , r4    = runif(n = 36, min = 0.93,   max = 7.43)
#'   , r5    = runif(n = 36, min = 1.12,   max = 7.00)
#'   , r6    = runif(n = 36, min = 0.99,   max = 7.93)
#'   , r7    = runif(n = 36, min = 1.51,   max = 7.42)
#'   , r8    = runif(n = 36, min = 2.20,   max = 9.15)
#'   , r9    = runif(n = 36, min = 2.64,   max = 9.37)
#'   , r10   = runif(n = 36, min = 2.80,   max = 11.34)
#'   , r11   = runif(n = 36, min = 3.01,   max = 12.41)
#'   , r12   = runif(n = 36, min = 2.78,   max = 13.68)
#'   , r13   = runif(n = 36, min = 3.23,   max = 14.96)
#'   )
#'
#' Data$Date <- as.Date(paste("01", Data$Date, sep = "-"), format = "%d-%b-%Y")
#' Data
#'
#' # Divisia monetary aggregates index using Barnett method
#' DMAIBarnett <- dmai(.data = Data, method = "Barnett")
#' DMAIBarnett
#'
#' # Divisia monetary aggregates index using Hancock method
#' DMAIHancock <- dmai(.data = Data, method = "Hancock")
#' DMAIHancock
#'
#' library(ggplot2)
#' ggplot(data = DMAIBarnett, mapping = aes(x = Date, y = DMAI)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = round(DMAI, 2)), vjust = "inward", hjust = "inward") +
#'   scale_x_date(
#'                 date_breaks = "6 months"
#'               , date_labels = "%b-%Y"
#'               , limits = c(min(DMAIBarnett$Date), max = max(DMAIBarnett$Date))) +
#'   theme_bw() +
#'   theme(axis.text.x  = element_text(angle = 90))
#'
#' ggplot(data = DMAIHancock, mapping = aes(x = Date, y = DMAI)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = round(DMAI, 2)), vjust = "inward", hjust = "inward") +
#'   scale_x_date(
#'                 date_breaks = "6 months"
#'               , date_labels = "%b-%Y"
#'               , limits = c(min(DMAIHancock$Date), max = max(DMAIHancock$Date))) +
#'   theme_bw() +
#'   theme(axis.text.x  = element_text(angle = 90))
#'
if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(
      ".data"
      ,"Date"
      , "R"
      , "Pi"
      , "St"
      , "StStar"
      , "StStar1"
      , "grp"
      , "r"
      , "val"
      , "value"
      , "var"
      , "W"
      , "W12"
      , "rB"
      , "x"
    )
  )
}

dmai <- function(.data, method = c("Barnett", "Hancock"), logbase = NULL){
  UseMethod("dmai")
}

#' @export
#' @rdname dmai

dmai.default <- function(.data, method = c("Barnett", "Hancock"), logbase = 10){
  if(method == "Hancock"){
    DMAI <-
      .data %>%
      tidyr::gather(val, var, -Date) %>%
      tidyr::extract(val, c("value", "grp"), regex = "([a-z]+)([0-9]+)") %>%
      tidyr::spread(value, var) %>%
      dplyr::mutate(grp = as.numeric(grp)) %>%
      dplyr::arrange(Date, grp) %>%
      dplyr::group_by(Date) %>%
      dplyr::mutate(
        rB = max(r)
        , W  = x*(rB - r)/sum(x*(rB - r))
      ) %>%
      dplyr::ungroup(Date) %>%
      dplyr::mutate(
        W12  = (W + dplyr::lag(x = W, n = length(unique(grp))))/2*(x/dplyr::lag(x = x, n = length(unique(grp))) - 1)
      ) %>%
      dplyr::group_by(Date) %>%
      dplyr::mutate(DMAI = sum(W12)*100) %>%
      dplyr::select(Date, DMAI) %>%
      dplyr::distinct()
  }

  else{
    logbase <- ifelse(is.null(logbase) | logbase == 10, 10, exp(1))

    DMAI <-
      .data %>%
      tidyr::gather(val, var, -Date) %>%
      tidyr::extract(val, c("value", "grp"), regex = "([a-z]+)([0-9]+)") %>%
      tidyr::spread(value, var) %>%
      dplyr::mutate(grp = as.numeric(grp)) %>%
      dplyr::arrange(Date, grp) %>%
      dplyr::group_by(Date) %>%
      dplyr::mutate(
        R    = max(r)
        , Pi   = (R-r)/(1+R)
        , St   = Pi*x / sum(Pi*x)
      ) %>%
      dplyr::ungroup(Date) %>%
      dplyr::mutate(
        StStar  = (St + dplyr::lag(x = St, n = length(unique(grp))))/2
        , StStar1 = StStar*(log(x = x, base = logbase) - log(dplyr::lag(x = x, n = length(unique(grp))), base = logbase))
      ) %>%
      dplyr::group_by(Date) %>%
      dplyr::mutate(DMAI = sum(StStar1)*100) %>%
      dplyr::select(Date, DMAI) %>%
      dplyr::distinct()
  }

  return(DMAI)
}
