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


## ----legacy-------------------------------------------------------------------
data(money_sim)
new <-
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
    , as_dt     = TRUE
    )
old <-
  dmai(
      .data     = money_sim
    , method    = c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE")
    , logbase   = 10
    , date      = "Date"
    , x         = NULL
    , r         = NULL
    , user_cost = NULL
    , benchmark = "rb"
    , percent   = TRUE
    , base      = 100
    , as_dt     = TRUE
    )
head(
    data.table::data.table(
        Date    = new$Date
      , natural = new$DMAI
      , base10  = old$DMAI
      , ratio   = new$DMAI / old$DMAI
      )
  )


## ----cpi----------------------------------------------------------------------
data(cpi_ch19)
cpi_ch19
tor <-
  dmai(
      .data     = cpi_ch19
    , method    = "Barnett"
    , logbase   = NULL
    , date      = "period"
    , x         = paste0("p", 1:6)
    , r         = NULL
    , user_cost = paste0("q", 1:6)
    , benchmark = NULL
    , percent   = TRUE
    , base      = 1
    , as_dt     = TRUE
    )
fis <-
  dmai(
      .data     = cpi_ch19
    , method    = "Fisher"
    , logbase   = NULL
    , date      = "period"
    , x         = paste0("p", 1:6)
    , r         = NULL
    , user_cost = paste0("q", 1:6)
    , benchmark = NULL
    , percent   = TRUE
    , base      = 1
    , as_dt     = FALSE
    )
data.table::data.table(
    period    = 1:5
  , laspeyres = round(x = cumprod(c(1, fis$fisher$laspeyres[-1])), digits = 4)
  , paasche   = round(x = cumprod(c(1, fis$fisher$paasche[-1])), digits = 4)
  , fisher    = round(x = fis$index$index, digits = 4)
  , tornqvist = round(x = tor$index, digits = 4)
  )


## ----reference----------------------------------------------------------------
ref <-
  dmai_reference(
      source   = NULL
    , evaluate = TRUE
    , level    = 1
    )
ref[, .(id, value, computed = round(x = computed, digits = 5), agrees)]


## ----reversal-----------------------------------------------------------------
f <-
  dmai(
      .data     = money_sim
    , method    = "Fisher"
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
max(abs(f$price$price * f$index$value - f$price$expenditure))


## ----bench--------------------------------------------------------------------
b <- data.table::fread(system.file("extdata", "bench.csv", package = "dmai"))
w <- data.table::dcast(data = b, formula = case ~ version, value.var = "median_ms")
v <- sort(unique(b$version))
w[, speedup := round(x = get(v[1]) / get(v[2]), digits = 1)]
w

