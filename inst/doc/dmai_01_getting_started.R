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


## ----data---------------------------------------------------------------------
data(money_sim)
head(money_sim[, c("Date", "x1", "x2", "x3", "r1", "r2", "r3", "rb")])


## ----fit----------------------------------------------------------------------
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


## ----fit-plot-----------------------------------------------------------------
plot(
    x     = fit
  , which = "index"
  )


## ----shares-------------------------------------------------------------------
plot(
    x     = fit
  , which = "shares"
  )


## ----interpret----------------------------------------------------------------
dmai_interpret(
    x   = fit
  , top = 3
  )


## ----compare------------------------------------------------------------------
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
cmp
plot(
    x     = cmp
  , which = "index"
  )


## ----user-cost----------------------------------------------------------------
dmai_user_cost(
    .data     = money_sim
  , date      = "Date"
  , x         = NULL
  , r         = NULL
  , benchmark = "rb"
  , percent   = TRUE
  )


## ----moments------------------------------------------------------------------
m <-
  dmai_moments(
      .data = fit
    )
plot(x = m)

