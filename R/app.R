#' Monetary aggregates in a web browser
#'
#' @description
#' \code{dmai_app()} starts the application of \pkg{dmai}: load the components of a monetary aggregate (the
#' simulated example \code{\link{money_sim}} or a CSV file), choose the quantity and rate columns and the benchmark
#' rate, compute the Divisia or another index, compare all methods, inspect the user costs, weights and Divisia
#' second moments. Every result page shows an interpretation computed from the results
#' (\code{\link{dmai_interpret}()}), the output as R prints it, formatted tables, plots and the R code that repeats the
#' analysis, each with a Copy button.
#'
#' @details
#' The application needs the suggested packages \pkg{shiny}, \pkg{bslib} and \pkg{DT}. Plots are sent to the browser
#' as WebP images when \pkg{magick} is installed (smaller than PNG; PNG otherwise) and can be downloaded as PNG, PDF
#' or WebP. Text is set in IBM Plex Sans and output in IBM Plex Mono when the browser can reach Google Fonts, in the
#' system fonts otherwise. A CSV file needs one row per date: a date column (read as a date when it looks like one)
#' and a quantity and a rate column for every component.
#'
#' @param launch.browser passed to \code{shiny::runApp()}: \code{TRUE} (default in an interactive session) opens the
#'   application in the web browser.
#' @param ... further arguments to \code{shiny::runApp()}, such as \code{port}.
#' @return \code{dmai_app()} is called for its side effect and returns the value of \code{shiny::runApp()} when the
#'   application is closed; \code{dmai_app_object()} returns the application object (for \code{shiny::runApp()},
#'   \code{shiny::testServer()} or deployment on a server).
#' @author Muhammad Yaseen (\email{myaseen208@@gmail.com})
#' @seealso \code{\link{dmai}}, \code{\link{dmai_compare}}, \code{\link{dmai_interpret}}
#' @examples
#' if (interactive()) {
#'   dmai_app(
#'       launch.browser = TRUE
#'     )
#' }
#' @export
dmai_app <- function(launch.browser = interactive(), ...) {
  .dmai_app_need()
  shiny::runApp(dmai_app_object(), launch.browser = launch.browser, ...)
}

#' @rdname dmai_app
#' @export
dmai_app_object <- function() {
  .dmai_app_need()
  shiny::shinyApp(ui = .dmai_app_ui(), server = .dmai_app_server)
}

.dmai_app_need <- function() {
  miss <- c("shiny", "bslib", "DT")[!vapply(c("shiny", "bslib", "DT"), requireNamespace, NA, quietly = TRUE)]
  if (length(miss)) .dmai_stop("dmai_app", "the application needs the package(s) ", paste(miss, collapse = ", "),
                              ": install.packages(c(", paste0("\"", miss, "\"", collapse = ", "), ")).")
}

#-------------------------------------------------------------------------------------------------------------------
# look: theme, fonts, styles, Copy buttons
#-------------------------------------------------------------------------------------------------------------------
.dmai_app_font <- function(family, kind = c("sans", "mono")) {
  fb <- switch(match.arg(kind),
               sans = c("system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
               mono = c("SFMono-Regular", "Menlo", "Consolas", "Liberation Mono", "DejaVu Sans Mono", "monospace"))
  do.call(bslib::font_collection, c(list(bslib::font_google(family, local = FALSE)), as.list(fb)))
}

.dmai_app_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#1f4e9e",
                  base_font = .dmai_app_font("IBM Plex Sans", "sans"),
                  heading_font = .dmai_app_font("IBM Plex Sans", "sans"),
                  code_font = .dmai_app_font("IBM Plex Mono", "mono"))
}

.dmai_app_head <- function() {
  shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste(
          "pre.shiny-text-output{font-family:var(--bs-font-monospace);font-size:.85em;line-height:1.5;white-space:pre;"
        , "  overflow-x:auto;background:#f6f8fa;border:1px solid #dde3ea;border-radius:6px;padding:10px 12px;}"
        , "table.dataTable thead th{background:#1f4e9e;color:#fff;font-weight:600;}"
        , ".dmai-bar{display:flex;gap:.4rem;justify-content:flex-end;margin-bottom:.4rem}"
        , ".dataTables_wrapper .dt-buttons{float:left;margin:0 1rem .5rem 0}"
        , ".dataTables_wrapper .dataTables_length{float:left;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_filter{float:right;margin-bottom:.5rem}"
        , ".dataTables_wrapper .dataTables_length label,.dataTables_wrapper .dataTables_filter label{display:inline-flex;"
        , "  align-items:center;gap:.4rem;font-weight:400;margin:0}"
        , ".dataTables_wrapper select,.dataTables_wrapper input[type=search]{display:inline-block;width:auto}"
        , ".dataTables_wrapper .dataTables_info{float:left;padding-top:.6rem;font-size:.9em}"
        , ".dataTables_wrapper .dataTables_paginate{float:right;padding-top:.4rem}"
        , ".dataTables_wrapper table.dataTable{clear:both}"
        , ".dataTables_wrapper::after{content:'';display:table;clear:both}"
        , ".dmai-plot img{max-width:100%;height:auto}")))
    , shiny::tags$script(shiny::HTML(paste(
          "$(document).on('click', '.dmai-copy', function() {"
        , "  var el = document.getElementById($(this).data('target')); if (!el) return;"
        , "  var txt = el.innerText || el.textContent, b = $(this);"
        , "  var done = function() { b.find('.dmai-copy-label').text('Copied'); setTimeout(function() { b.find('.dmai-copy-label').text('Copy'); }, 1500); };"
        , "  if (navigator.clipboard && window.isSecureContext) { navigator.clipboard.writeText(txt).then(done); return; }"
        , "  var ta = document.createElement('textarea'); ta.value = txt; document.body.appendChild(ta); ta.select();"
        , "  document.execCommand('copy'); document.body.removeChild(ta); done();"
        , "});", sep = "\n")))
    )
}

.dmai_copy_bar <- function(target) {
  shiny::div(class = "dmai-bar",
             shiny::tags$button(type = "button", class = "btn btn-outline-secondary btn-sm dmai-copy",
                                `data-target` = target, shiny::icon("copy"),
                                shiny::span(class = "dmai-copy-label", "Copy")))
}

.dmai_btn <- "btn btn-sm btn-outline-secondary me-1"

# a table for the application: Copy, CSV and Excel buttons, all rows on request, numbers rounded
.dmai_dt <- function(d, title = "table") {
  d <- as.data.frame(d, check.names = FALSE)
  num <- which(vapply(d, is.numeric, NA))
  t <- DT::datatable(d, rownames = FALSE, extensions = "Buttons", style = "auto",
                     class = "compact stripe hover cell-border",
                     options = list(pageLength = 15L, scrollX = TRUE, dom = "Blfrtip",
                                    lengthMenu = list(c(15L, 50L, -1L), c("15", "50", "All")),
                                    buttons = list(list(extend = "copy", title = NULL, className = .dmai_btn),
                                                   list(extend = "csv", title = title, className = .dmai_btn),
                                                   list(extend = "excel", title = title, className = .dmai_btn))))
  pcol  <- num[grepl("^p_|^prob|p_value|^Probability", names(d)[num])]
  whole <- setdiff(num[vapply(d[num], function(v) all(is.na(v) | v == round(v)), NA)], pcol)
  dec   <- setdiff(num, c(whole, pcol))
  if (length(pcol)) t <- DT::formatRound(t, pcol, digits = 3L)
  if (length(dec)) t <- DT::formatSignif(t, dec, digits = 5L)
  if (length(whole)) t <- DT::formatRound(t, whole, digits = 0L)
  t
}

# a ggplot as an image for renderImage(): WebP through magick when installed, PNG otherwise
.dmai_plot_file <- function(p, width = 900, height = 600, res = 110) {
  png <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(png, width = width, height = height, res = res) else
    grDevices::png(png, width = width, height = height, res = res)
  print(p)
  grDevices::dev.off()
  if (.dmai_webp_ok()) {
    webp <- sub("\\.png$", ".webp", png)
    ok <- tryCatch({
      magick::image_write(magick::image_read(png), webp, format = "webp", quality = 100,
                          defines = c(`webp:lossless` = "true"))
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(list(src = webp, contentType = "image/webp", width = "100%", alt = "plot"))
  }
  list(src = png, contentType = "image/png", width = "100%", alt = "plot")
}

# a card with tabs: Interpretation, Output, Table, Plot (each with a Copy button where it is text)
.dmai_result_tabs <- function(id, plot = TRUE, table = TRUE, code = FALSE, extra = NULL) {
  ns <- shiny::NS(id)
  tabs <- list(
      bslib::nav_panel("Interpretation", .dmai_copy_bar(ns("int_text")), shiny::uiOutput(ns("int")),
                       shiny::tags$pre(id = ns("int_text"), class = "shiny-text-output", style = "display:none"))
    , if (plot) bslib::nav_panel("Plot", extra, shiny::div(class = "dmai-plot", shiny::imageOutput(ns("plot"), height = "auto")),
                                 shiny::div(shiny::downloadButton(ns("png"), "PNG", class = "btn-sm"),
                                            shiny::downloadButton(ns("pdf"), "PDF", class = "btn-sm"),
                                            shiny::downloadButton(ns("webp"), "WebP", class = "btn-sm")))
    , if (table) bslib::nav_panel("Table", DT::DTOutput(ns("table")))
    , bslib::nav_panel("Output", .dmai_copy_bar(ns("out")), shiny::verbatimTextOutput(ns("out")))
    , if (code) bslib::nav_panel("R code", .dmai_copy_bar(ns("code")), shiny::verbatimTextOutput(ns("code")))
    )
  do.call(bslib::navset_card_tab, c(list(id = ns("tabs")), Filter(Negate(is.null), tabs)))
}

.dmai_int_html <- function(it) {
  li <- function(s) if (length(s)) shiny::tags$ul(lapply(s, shiny::tags$li)) else NULL
  shiny::tagList(
      shiny::div(class = "alert alert-primary", shiny::strong(it$headline))
    , if (length(it$findings)) shiny::tagList(shiny::h6("Findings"), li(it$findings))
    , if (length(it$recommendations)) shiny::tagList(shiny::h6("Recommendations"), li(it$recommendations))
    , if (length(it$cautions)) shiny::tagList(shiny::h6("Cautions"), li(it$cautions))
    , shiny::tags$details(shiny::tags$summary("How this is read"), shiny::p(it$guide))
    )
}

# the server side of a result card: res() a dmai_result, table_fun(res) a data frame, plot_fun(res) a ggplot
.dmai_result_server <- function(id, res, table_fun = NULL, plot_fun = NULL, code_fun = NULL, hb = shiny::reactive(TRUE)) {
  force(res)
  force(table_fun)
  force(plot_fun)
  force(code_fun)
  shiny::moduleServer(id, function(input, output, session) {
    it <- shiny::reactive({
      shiny::req(res())
      dmai_interpret(res())
    })
    output$int <- shiny::renderUI(.dmai_int_html(it()))
    output$int_text <- shiny::renderText(paste(format(it(), style = "text", width = 100L), collapse = "\n"))
    output$out <- shiny::renderPrint({
      shiny::req(res())
      op <- options(width = 120L)
      on.exit(options(op))
      print(res())
    })
    if (!is.null(table_fun)) output$table <- DT::renderDT({
      shiny::req(res())
      .dmai_dt(table_fun(res()), id)
    })
    if (!is.null(plot_fun)) {
      pl <- shiny::reactive({
        shiny::req(res())
        plot_fun(res(), input)
      })
      output$plot <- shiny::renderImage(.dmai_plot_file(pl()), deleteFile = TRUE)
      output$png <- shiny::downloadHandler(paste0(id, ".png"), function(file)
        ggplot2::ggsave(file, pl(), width = 10, height = 7, dpi = 200))
      output$pdf <- shiny::downloadHandler(paste0(id, ".pdf"), function(file)
        ggplot2::ggsave(file, pl(), width = 10, height = 7))
      output$webp <- shiny::downloadHandler(paste0(id, ".webp"), function(file) dmai_save(pl(), file, width = 10, height = 7))
    }
    if (!is.null(code_fun)) output$code <- shiny::renderText(code_fun())
  })
}


#-------------------------------------------------------------------------------------------------------------------
# pages
#-------------------------------------------------------------------------------------------------------------------
.dmai_app_ui <- function() {
  side <- function(...) bslib::sidebar(width = 320, ...)
  methods <- c(`Divisia (Barnett 1980)` = "Barnett", `Divisia, simple growth (Hancock)` = "Hancock",
               `Fisher ideal` = "Fisher", `Simple sum` = "SimpleSum", `Currency equivalent` = "CE")
  bslib::page_navbar(
      title = "dmai", id = "nav", theme = .dmai_app_theme(), fillable = FALSE, header = .dmai_app_head()
    , bslib::nav_panel("Home", .dmai_app_home())
    , bslib::nav_panel("Data", bslib::layout_sidebar(
          sidebar = side(
              shiny::radioButtons("source", "Data", c(`Example: money_sim (simulated, monthly 2005 to 2024)` = "example",
                                                      `CSV file` = "csv"))
            , shiny::conditionalPanel("input.source == 'csv'", shiny::fileInput("file", "CSV file (one row per date)",
                                                                                accept = ".csv"))
            , shiny::selectInput("date", "Date column", NULL)
            , shiny::selectizeInput("x", "Quantity columns", NULL, multiple = TRUE)
            , shiny::selectizeInput("r", "Rate columns (same order)", NULL, multiple = TRUE)
            , shiny::selectInput("bench", "Benchmark rate", NULL)
            , shiny::checkboxInput("percent", "Rates in percent", TRUE))
        , bslib::card(bslib::card_header("The data"), .dmai_copy_bar("data_summary"),
                      shiny::verbatimTextOutput("data_summary"), DT::DTOutput("data_table"))))
    , bslib::nav_panel("Index", bslib::layout_sidebar(
          sidebar = side(
              shiny::radioButtons("method", "Method", methods)
            , shiny::numericInput("base", "Index at the first date", 100, min = 0)
            , shiny::selectInput("logbase", "Logarithm of the growth", c(`natural (percent)` = "e", `base 10 (version 0.5.0)` = "10",
                                                                         `base 2` = "2"))
            , shiny::actionButton("run", "Compute", class = "btn-primary w-100", icon = shiny::icon("play")))
        , .dmai_result_tabs("fit", code = TRUE, extra = shiny::selectInput("fit_plot", NULL,
                            c(`index and simple sum` = "index", growth = "growth", `weights (shares)` = "shares",
                              `user costs` = "user_cost", `second moments` = "moments", `dual price index` = "price")))))
    , bslib::nav_panel("Compare", bslib::layout_sidebar(
          sidebar = side(
              shiny::checkboxGroupInput("methods", "Methods", methods, selected = unname(methods))
            , shiny::radioButtons("cmp_plot", "Plot", c(index = "index", growth = "growth"), inline = TRUE)
            , shiny::actionButton("compare", "Compare", class = "btn-primary w-100"))
        , .dmai_result_tabs("cmp", code = FALSE)))
    , bslib::nav_panel("User costs", bslib::layout_sidebar(
          sidebar = side(shiny::radioButtons("uc_plot", "Plot", c(`weights (shares)` = "shares", `user costs` = "user_cost")),
                         shiny::helpText("For the data, columns and benchmark of the Data page."))
        , .dmai_result_tabs("uc")))
    , bslib::nav_panel("Second moments", bslib::layout_sidebar(
          sidebar = side(shiny::helpText("Divisia mean and variance of the growth of the components (Barnett and Serletis 1990).")),
          .dmai_result_tabs("mom")))
    , bslib::nav_panel("References", bslib::card(bslib::card_header("Sources of the methods"), .dmai_copy_bar("refs"),
                                                 shiny::verbatimTextOutput("refs")))
    )
}

.dmai_app_home <- function() {
  card <- function(title, text) bslib::card(bslib::card_header(title), shiny::p(text))
  shiny::tagList(
      shiny::h4("Divisia monetary aggregates")
    , shiny::p(paste("This application builds a monetary aggregate from its components. The Divisia index weights",
                     "each component by its share in the expenditure on monetary services, priced by its user cost:",
                     "the interest given up by holding it instead of a benchmark asset. The simple sum, the usual",
                     "official aggregate, weights every component equally."))
    , bslib::layout_column_wrap(width = 1 / 3,
          card("1. Data", "The simulated example or a CSV file; date, quantity and rate columns; benchmark rate.")
        , card("2. Index", "One method: index, growth, weights, user costs, interpretation and R code.")
        , card("3. Compare", "Divisia, Fisher, simple sum and currency equivalent side by side.")
        , card("4. User costs", "The user cost and weight of every component over time.")
        , card("5. Second moments", "How differently the components grow at each date.")
        , card("6. References", "The articles behind the methods."))
    )
}

.dmai_app_example <- function() {
  e <- new.env()
  utils::data("money_sim", package = "dmai", envir = e)
  e$money_sim
}

# a date column read from a CSV file: as Date when every value parses
.dmai_app_dates <- function(v) {
  if (inherits(v, "Date") || is.numeric(v)) return(v)
  for (f in c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%b-%Y", "%b-%Y")) {
    d <- as.Date(if (f == "%b-%Y") paste0("01-", v) else v, format = if (f == "%b-%Y") "%d-%b-%Y" else f)
    if (!anyNA(d)) return(d)
  }
  v
}

.dmai_app_server <- function(input, output, session) {
  raw <- shiny::reactive({
    if (identical(input$source, "csv")) {
      shiny::req(input$file)
      d <- data.table::fread(input$file$datapath, data.table = FALSE)
    } else d <- .dmai_app_example()
    d
  })
  shiny::observeEvent(raw(), {
    nm <- names(raw())
    num <- nm[vapply(raw(), is.numeric, NA)]
    dcol <- if ("Date" %in% nm) "Date" else nm[1L]
    xs <- .dmai_default_columns(nm, "x")
    rs <- .dmai_default_columns(nm, "r")
    shiny::updateSelectInput(session, "date", choices = nm, selected = dcol)
    shiny::updateSelectizeInput(session, "x", choices = num, selected = xs)
    shiny::updateSelectizeInput(session, "r", choices = num, selected = rs)
    shiny::updateSelectInput(session, "bench", choices = c(`largest component rate` = "", setdiff(num, c(xs, rs))),
                             selected = if ("rb" %in% num) "rb" else "")
  })
  dat <- shiny::reactive({
    d <- raw()
    shiny::req(input$date %in% names(d))
    d[[input$date]] <- .dmai_app_dates(d[[input$date]])
    d
  })
  args <- shiny::reactive(list(.data = dat(), date = input$date, x = input$x, r = input$r,
                               benchmark = if (nzchar(input$bench)) input$bench else NULL,
                               percent = isTRUE(input$percent)))
  output$data_summary <- shiny::renderPrint({
    d <- dat()
    cat(sprintf("%d dates, %s to %s; %d quantity and %d rate columns\n", nrow(d), format(min(d[[input$date]])),
                format(max(d[[input$date]])), length(input$x), length(input$r)))
    if (length(input$x) != length(input$r)) cat("Choose one rate column for every quantity column, in the same order.\n")
    if (length(input$x)) print(summary(d[, input$x, drop = FALSE]))
  })
  output$data_table <- DT::renderDT(.dmai_dt(dat(), "data"))

  run <- function(expr) {
    r <- tryCatch(shiny::withProgress(message = "Computing", value = 0.5, expr), error = function(e) e)
    if (inherits(r, "error")) {
      shiny::showNotification(conditionMessage(r), type = "error", duration = 10)
      return(NULL)
    }
    r
  }
  fit <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$run, {
    lb <- switch(input$logbase, e = NULL, `10` = 10, `2` = 2)
    fit(run(do.call(dmai, c(args(), list(method = input$method, logbase = lb, base = input$base, as_dt = FALSE)))))
  })
  .dmai_result_server("fit", fit, function(x) x$index, function(x, i) plot(x, which = input$fit_plot),
                      shiny::reactive(.dmai_app_code(input)))
  cmp <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$compare, {
    cmp(run(do.call(dmai_compare, c(args(), list(methods = input$methods, base = input$base)))))
  })
  .dmai_result_server("cmp", cmp, function(x) data.table::dcast(x$indexes, stats::as.formula(paste(x$settings$date,
                                                                  "~ method")), value.var = "index"),
                      function(x, i) plot(x, which = input$cmp_plot))
  uc <- shiny::reactive(run(do.call(dmai_user_cost, args())))
  .dmai_result_server("uc", uc, function(x) x$components, function(x, i) plot(x, which = input$uc_plot))
  mom <- shiny::reactive(run(do.call(dmai_moments, args())))
  .dmai_result_server("mom", mom, function(x) x$moments, function(x, i) plot(x))
  output$refs <- shiny::renderPrint({
    op <- options(width = 110L)
    on.exit(options(op))
    print(dmai_cite())
  })
}

# the R code of the current index, every argument on its own line
.dmai_app_code <- function(input) {
  csv <- identical(input$source, "csv")
  q <- function(v) if (length(v) == 1L) sprintf("\"%s\"", v) else sprintf("c(%s)", paste0("\"", v, "\"", collapse = ", "))
  load <- if (csv) c("dat <- data.table::fread(\"data.csv\", data.table = FALSE)",
                     sprintf("dat$%s <- as.Date(dat$%s)", input$date, input$date)) else
    c("data(money_sim)", "dat <- money_sim")
  vals <- c(.data = "dat", method = q(input$method),
            logbase = switch(input$logbase, e = "NULL", input$logbase), date = q(input$date), x = q(input$x),
            r = q(input$r), user_cost = "NULL", benchmark = if (nzchar(input$bench)) q(input$bench) else "NULL",
            percent = as.character(isTRUE(input$percent)), base = format(input$base), as_dt = "FALSE")
  w <- max(nchar(names(vals)))
  body <- paste0("    ", c(" ", rep(",", length(vals) - 1L)), " ", formatC(names(vals), width = -w), " = ", vals,
                 collapse = "\n")
  paste0("library(dmai)\n", paste0(load, "\n", collapse = ""), "fit <-\n  dmai(\n", body,
         "\n    )\nfit\nplot(x = fit)\ndmai_interpret(\n    x   = fit\n  , top = 3\n  )\n")
}
