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


## ----launch, eval = FALSE-----------------------------------------------------
# dmai_app(
#     launch.browser = TRUE
#   )


## ----shot-index, echo = FALSE, out.width = "100%"-----------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-index.jpg" else "figures/app-index.webp")


## ----shot-plot, echo = FALSE, out.width = "100%"------------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-plot.jpg" else "figures/app-plot.webp")


## ----shot-compare, echo = FALSE, out.width = "100%"---------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-compare.jpg" else "figures/app-compare.webp")


## ----shot-usercost, echo = FALSE, out.width = "100%"--------------------------
knitr::include_graphics(path = if (knitr::is_latex_output()) "figures/app-usercost.jpg" else "figures/app-usercost.webp")


## ----code---------------------------------------------------------------------
settings <-
  list(
      source  = "example"
    , method  = "Barnett"
    , logbase = "e"
    , date    = "Date"
    , x       = paste0("x", 1:8)
    , r       = paste0("r", 1:8)
    , bench   = "rb"
    , percent = TRUE
    , base    = 100
    )
cat(dmai:::.dmai_app_code(input = settings))


## ----deploy, eval = FALSE-----------------------------------------------------
# # app.R on a Shiny server
# dmai::dmai_app_object()

