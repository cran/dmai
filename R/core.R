#-------------------------------------------------------------------------------------------------------------------
# Computations shared by the functions of the package: the input (wide data, one column per component), user costs,
# shares and the index formulas. Everything works on a T x n matrix of quantities X (dates in rows, components in
# columns), the matching rates (decimal) or user costs, and the benchmark rate.
#-------------------------------------------------------------------------------------------------------------------

# Columns x1, x2, ... and r1, r2, ... (the interface of version 0.5.0), paired by their number
.dmai_default_columns <- function(nm, prefix) {
  m <- regmatches(nm, regexec(paste0("^", prefix, "([0-9]+)$"), nm))
  ok <- lengths(m) == 2L
  if (!any(ok)) return(character(0))
  k <- as.integer(vapply(m[ok], `[`, "", 2L))
  nm[ok][order(k)]
}

# The input of every function: dates, quantities, rates, user costs and benchmark as matrices and vectors.
# `benchmark`: NULL (the largest component rate), a column name, a number or a vector of length nrow(.data).
.dmai_input <- function(.data, date, x, r, user_cost, benchmark, percent, fn, need_rates = FALSE) {
  .data <- .dmai_check_data(.data, fn)
  d <- data.table::as.data.table(.data)
  if (!is.character(date) || length(date) != 1L || !date %in% names(d)) {
    .dmai_stop(fn, "`date` must name the date column of `.data` (default \"Date\").")
  }
  percent <- .dmai_check_flag(percent, "percent", fn)
  if (is.null(x)) x <- .dmai_default_columns(names(d), "x")
  if (!length(x)) .dmai_stop(fn, "no quantity columns: name them x1, x2, ... or give `x`.")
  if (!is.character(x) || !all(x %in% names(d))) .dmai_stop(fn, "`x` must name columns of `.data`.")
  n <- length(x)
  if (n < 2L) .dmai_stop(fn, "at least two components are needed.")
  if (!is.null(user_cost)) {
    if (!is.character(user_cost) || length(user_cost) != n || !all(user_cost %in% names(d))) {
      .dmai_stop(fn, "`user_cost` must name one column of `.data` for every column of `x`.")
    }
  }
  if (is.null(r) && (is.null(user_cost) || need_rates)) r <- .dmai_default_columns(names(d), "r")
  if (!is.null(r) && length(r)) {
    if (!is.character(r) || length(r) != n || !all(r %in% names(d))) {
      .dmai_stop(fn, "`r` must name one rate column of `.data` for every column of `x` (", n, " columns).")
    }
  } else r <- NULL
  if (is.null(r) && is.null(user_cost)) .dmai_stop(fn, "no rate columns: name them r1, r2, ... or give `r` or `user_cost`.")
  if (need_rates && is.null(r)) .dmai_stop(fn, "this method needs the rates of return of the components (`r`).")

  d <- d[order(d[[date]])]
  dates <- d[[date]]
  if (anyDuplicated(dates)) .dmai_stop(fn, "the dates in `", date, "` must be distinct.")
  X <- as.matrix(d[, x, with = FALSE])
  storage.mode(X) <- "double"
  if (anyNA(X) || any(X <= 0)) .dmai_stop(fn, "the quantities must be positive and not missing.")
  colnames(X) <- x
  scale <- if (percent) 100 else 1
  Rr <- NULL
  if (!is.null(r)) {
    Rr <- as.matrix(d[, r, with = FALSE]) / scale
    storage.mode(Rr) <- "double"
    if (anyNA(Rr)) .dmai_stop(fn, "the rates must not be missing.")
    colnames(Rr) <- x
  }
  # benchmark rate
  top <- if (is.null(Rr)) NULL else apply(Rr, 1L, max)
  bench_given <- !is.null(benchmark)
  if (bench_given) {
    b <- if (is.character(benchmark) && length(benchmark) == 1L) {
      if (!benchmark %in% names(d)) .dmai_stop(fn, "`benchmark`: no column \"", benchmark, "\" in `.data`.")
      d[[benchmark]]
    } else if (is.numeric(benchmark) && length(benchmark) %in% c(1L, nrow(d))) {
      rep_len(benchmark, nrow(d))
    } else .dmai_stop(fn, "`benchmark` must be NULL, a column name, a number or one value per date.")
    b <- as.numeric(b) / scale
    if (anyNA(b)) .dmai_stop(fn, "the benchmark rate must not be missing.")
    R <- if (is.null(top)) b else pmax(b, top)
    envelope <- if (is.null(top)) rep(FALSE, length(b)) else top > b
  } else {
    R <- if (is.null(top)) rep(NA_real_, length(dates)) else top
    envelope <- rep(FALSE, length(dates))
  }
  # user costs: (R - r) / (1 + R), or given
  if (!is.null(user_cost)) {
    U <- as.matrix(d[, user_cost, with = FALSE])
    storage.mode(U) <- "double"
    if (anyNA(U) || any(U < 0)) .dmai_stop(fn, "the user costs must be non-negative and not missing.")
  } else {
    U <- (R - Rr) / (1 + R)
  }
  colnames(U) <- x
  E <- rowSums(U * X)
  if (any(E <= 0)) .dmai_stop(fn, "every date needs a component with a positive user cost (a rate below the benchmark).")
  list(date = dates, date_name = date, X = X, r = Rr, U = U, R = R, envelope = envelope, x = x, r_names = r,
       user_cost_names = user_cost, benchmark = if (bench_given) benchmark else NULL, percent = percent,
       T = nrow(X), n = n, given_cost = !is.null(user_cost))
}

# expenditure shares s_it = u_it x_it / sum_j u_jt x_jt and their averages over t - 1 and t
.dmai_shares <- function(inp) {
  S <- inp$U * inp$X / rowSums(inp$U * inp$X)
  Sbar <- rbind(NA_real_, (S[-1L, , drop = FALSE] + S[-inp$T, , drop = FALSE]) / 2)
  list(S = S, Sbar = Sbar)
}

# the link (ratio Q_t / Q_{t-1}) of every method; NA for the first date
.dmai_links <- function(inp, method) {
  X <- inp$X
  T <- inp$T
  dl <- rbind(NA_real_, diff(log(X)))
  sh <- .dmai_shares(inp)
  out <- switch(method,
    Barnett   = list(link = exp(rowSums(sh$Sbar * dl))),
    Hancock   = list(link = 1 + rowSums(sh$Sbar * rbind(NA_real_, X[-1L, , drop = FALSE] / X[-T, , drop = FALSE] - 1))),
    Fisher    = {
      U <- inp$U
      lasp <- c(NA_real_, rowSums(U[-T, , drop = FALSE] * X[-1L, , drop = FALSE]) /
                  rowSums(U[-T, , drop = FALSE] * X[-T, , drop = FALSE]))
      paas <- c(NA_real_, rowSums(U[-1L, , drop = FALSE] * X[-1L, , drop = FALSE]) /
                  rowSums(U[-1L, , drop = FALSE] * X[-T, , drop = FALSE]))
      list(link = sqrt(lasp * paas), laspeyres = lasp, paasche = paas)
    },
    SimpleSum = {
      s <- rowSums(X)
      list(link = c(NA_real_, s[-1L] / s[-T]), level = s)
    },
    CE        = {
      if (any(inp$R <= 0)) .dmai_stop("dmai", "the currency equivalent index needs a positive benchmark rate.")
      ce <- rowSums((inp$R - inp$r) / inp$R * X)
      list(link = c(NA_real_, ce[-1L] / ce[-T]), level = ce)
    })
  out$shares <- sh
  out
}

# index level from the links: base at the first date; `value` in the units of x (equal to the simple sum at the
# first date for the quantity indexes, the aggregate itself for the simple sum and the currency equivalent)
.dmai_levels <- function(link, base, first_value, level = NULL) {
  idx <- base * cumprod(c(1, link[-1L]))
  value <- if (is.null(level)) first_value * idx / base else level
  list(index = idx, value = value)
}

.dmai_methods <- c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE")

# the long table of components: date, component, quantity, rate, user cost, share
.dmai_components_table <- function(inp, sh = .dmai_shares(inp)) {
  dt <- data.table::data.table(
      date      = rep(inp$date, times = inp$n)
    , component = factor(rep(inp$x, each = inp$T), levels = inp$x)
    , quantity  = as.vector(inp$X)
    , rate      = if (is.null(inp$r)) NA_real_ else as.vector(inp$r) * (if (inp$percent) 100 else 1)
    , user_cost = as.vector(inp$U)
    , share     = as.vector(sh$S)
    )
  data.table::setnames(dt, "date", inp$date_name)
  dt
}

# growth of an index in percent: 100 log(Q_t / Q_{t-1}) / log(logbase), or 100 (Q_t / Q_{t-1} - 1) for Hancock
.dmai_growth <- function(link, method, logbase) {
  if (method == "Hancock") 100 * (link - 1) else 100 * log(link) / log(logbase)
}

# periods per year from the spacing of the dates (for the interpretation only)
.dmai_periods_per_year <- function(dates) {
  if (!inherits(dates, c("Date", "POSIXt")) || length(dates) < 2L) return(NA_real_)
  gap <- stats::median(as.numeric(diff(as.Date(dates))))
  cand <- c(365.25, 182.6, 91.3, 30.4, 7)
  ppy <- c(1, 2, 4, 12, 52)
  ppy[which.min(abs(cand - gap))]
}
