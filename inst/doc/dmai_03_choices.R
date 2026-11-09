## ----setup, include = FALSE---------------------------------------------------
# HTML figures are drawn as PNG and kept as WebP: lossless, or lossy at quality 90 when that is less than half the
# size. The PDF figures stay vector graphics. Any failure of magick (not installed, no WebP, or a broken
# installation whose system libraries are missing) keeps the PNG
webp <- function(path) {
  if (knitr::is_latex_output() || !grepl(pattern = "[.]png$", x = path)) return(path)
  tryCatch(
      {
        if (!requireNamespace("magick", quietly = TRUE) || !isTRUE(magick::magick_config()$webp)) return(path)
        img <- magick::image_read(path)
        out <- sub(pattern = "[.]png$", replacement = ".webp", x = path)
        tmp <- tempfile(fileext = ".webp")
        magick::image_write(
            image   = img
          , path    = out
          , format  = "webp"
          , quality = 100
          , defines = c(`webp:lossless` = "true")
          )
        magick::image_write(image = img, path = tmp, format = "webp", quality = 90)
        if (file.size(tmp) < file.size(out) / 2) file.copy(from = tmp, to = out, overwrite = TRUE)
        unlink(c(path, tmp))
        out
      }
    , error = function(e) path
    )
}
knitr::opts_chunk$set(collapse = FALSE, comment = "", fig.width = 7, fig.height = 4.2,
                      dev = if (knitr::is_latex_output()) "cairo_pdf" else "ragg_png", dpi = 96,
                      fig.process = webp)
if (requireNamespace("dmai", quietly = TRUE)) library(dmai) else
  pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)


## ----bench--------------------------------------------------------------------
data(money_sim)
bench <- list(
    `largest component rate` = NULL
  , `bond yield (rb)`        = "rb"
  , `bond yield + 1 point`   = money_sim$rb + 1
  , `bond yield + 3 points`  = money_sim$rb + 3
  )
growth <-
  vapply(
      X         = bench
    , FUN       = function(b) {
        d <-
          dmai(
              .data     = money_sim
            , method    = "Barnett"
            , logbase   = NULL
            , date      = "Date"
            , x         = NULL
            , r         = NULL
            , user_cost = NULL
            , benchmark = b
            , percent   = TRUE
            , base      = 100
            , as_dt     = TRUE
            )
        100 * ((d$index[nrow(d)] / d$index[1L])^(12 / (nrow(d) - 1)) - 1)
      }
    , FUN.VALUE = numeric(length = 1)
    )
round(x = growth, digits = 2)


## ----bench-weights------------------------------------------------------------
w <- function(b) {
  u <-
    dmai_user_cost(
        .data     = money_sim
      , date      = "Date"
      , x         = NULL
      , r         = NULL
      , benchmark = b
      , percent   = TRUE
      )$components
  u[, .(share = mean(share)), by = component]
}
merge(w(NULL), w("rb"), by = "component", suffixes = c("_largest_rate", "_bond_yield"))


## ----methods------------------------------------------------------------------
cmp <-
  dmai_compare(
      .data     = money_sim
    , methods   = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE")
    , logbase   = NULL
    , date      = "Date"
    , x         = NULL
    , r         = NULL
    , user_cost = NULL
    , benchmark = "rb"
    , percent   = TRUE
    , base      = 100
    )
cmp$summary
plot(
    x     = cmp
  , which = "growth"
  )


## ----gap----------------------------------------------------------------------
fit <-
  dmai(
      .data     = money_sim
    , method    = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE")
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
gap <- fit$simple_sum$growth - fit$index$DMAI
sd  <-
  dmai_moments(
      .data = fit
    )$moments$sd
cor(x = abs(gap), y = sd, use = "complete.obs")


## ----gap-plot-----------------------------------------------------------------
d <- data.table::data.table(Date = fit$index$Date, gap = gap, sd = sd)[!is.na(gap)]
ggplot2::ggplot(data = d, mapping = ggplot2::aes(x = sd, y = abs(gap))) +
  ggplot2::geom_point(colour = "#1f4e9e", alpha = 0.6) +
  ggplot2::labs(x = "Divisia standard deviation of component growth (percent)",
                y = "|simple sum - Divisia growth| (points)") +
  dmai_theme(
      base_size       = 11
    , base_family     = "serif"
    , ticks_inward    = TRUE
    , legend_position = "bottom"
    )

