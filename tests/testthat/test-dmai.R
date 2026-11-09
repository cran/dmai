# a small hand-computed case: two components, three dates
d3 <- data.frame(Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
                 x1 = c(100, 110, 121), x2 = c(200, 200, 220), r1 = c(0, 0, 0), r2 = c(2, 2, 3), rb = c(5, 5, 6))

test_that("the Divisia index follows its formula", {
  f <- dmai(d3, benchmark = "rb", as_dt = FALSE)
  R <- c(5, 5, 6) / 100
  u <- cbind((R - 0) / (1 + R), (R - c(2, 2, 3) / 100) / (1 + R))
  X <- cbind(d3$x1, d3$x2)
  s <- u * X / rowSums(u * X)
  sb <- (s[-1, ] + s[-3, ]) / 2
  g <- rowSums(sb * diff(log(X)))
  expect_equal(f$index$DMAI, c(NA, 100 * g))
  expect_equal(f$index$index, 100 * cumprod(c(1, exp(g))))
  expect_equal(f$index$value[1L], 300)
  expect_equal(f$components[component == "x2"]$share, s[, 2])
  expect_equal(f$components[component == "x2"]$user_cost, u[, 2])
  expect_result(f, "index", c("index", "components", "benchmark", "price", "moments", "simple_sum", "settings"))
})

test_that("Fisher, simple sum and currency equivalent follow their formulas", {
  R <- c(5, 5, 6) / 100
  r <- cbind(0, c(2, 2, 3) / 100)
  u <- (R - r) / (1 + R)
  X <- cbind(d3$x1, d3$x2)
  f <- dmai(d3, method = "Fisher", benchmark = "rb", as_dt = FALSE)
  L <- rowSums(u[-3, ] * X[-1, ]) / rowSums(u[-3, ] * X[-3, ])
  P <- rowSums(u[-1, ] * X[-1, ]) / rowSums(u[-1, ] * X[-3, ])
  expect_equal(f$fisher$fisher[-1], sqrt(L * P))
  expect_equal(f$index$DMAI[-1], 100 * log(sqrt(L * P)))
  ss <- dmai(d3, method = "SimpleSum", benchmark = "rb")
  expect_equal(ss$value, rowSums(X))
  ce <- dmai(d3, method = "CE", benchmark = "rb")
  expect_equal(ce$value, rowSums((R - r) / R * X))
  h <- dmai(d3, method = "Hancock", benchmark = "rb", as_dt = FALSE)
  s <- u * X / rowSums(u * X)
  expect_equal(h$index$DMAI[-1], 100 * rowSums((s[-1, ] + s[-3, ]) / 2 * (X[-1, ] / X[-3, ] - 1)))
})

test_that("factor reversal: the dual price times the aggregate is the expenditure", {
  for (m in c("Barnett", "Fisher")) {
    f <- dmai(money_sim, method = m, benchmark = "rb", as_dt = FALSE)
    expect_equal(f$price$price * f$index$value, f$price$expenditure)
  }
  # for the Fisher index the implicit price index is the Fisher price index of the user costs
  f <- dmai(cpi_ch19, method = "Fisher", date = "period", x = paste0("q", 1:6), user_cost = paste0("p", 1:6),
            base = 1, as_dt = FALSE)
  pf <- dmai(cpi_ch19, method = "Fisher", date = "period", x = paste0("p", 1:6), user_cost = paste0("q", 1:6), base = 1)
  expect_equal(f$price$price_index, pf$index)
})

test_that("Divisia and Fisher agree closely; the shares do not depend on 1 / (1 + R)", {
  b <- dmai(money_sim, method = "Barnett", benchmark = "rb")
  f <- dmai(money_sim, method = "Fisher", benchmark = "rb")
  expect_lt(max(abs(b$index / f$index - 1)), 1e-3)
  p <- dmai(money_sim, benchmark = "rb", percent = TRUE)
  d <- money_sim
  d[, c(paste0("r", 1:8), "rb")] <- d[, c(paste0("r", 1:8), "rb")] / 100
  q <- dmai(d, benchmark = "rb", percent = FALSE)
  expect_equal(p$DMAI, q$DMAI)
})

test_that("the benchmark: largest component rate, a column, a number, the upper envelope", {
  a <- dmai(d3, as_dt = FALSE)
  expect_equal(a$benchmark$benchmark, c(2, 2, 3))
  expect_true(all(a$components[component == "x2"]$user_cost == 0))
  b <- dmai(d3, benchmark = 2.5, as_dt = FALSE)
  expect_equal(b$benchmark$benchmark, c(2.5, 2.5, 3))
  expect_identical(b$benchmark$envelope, c(FALSE, FALSE, TRUE))
  expect_equal(dmai(d3, benchmark = c(5, 5, 6))$DMAI, dmai(d3, benchmark = "rb")$DMAI)
})

test_that("columns can be named and the data need not be sorted", {
  d <- d3[3:1, ]
  names(d) <- c("when", "cur", "dep", "rcur", "rdep", "bond")
  a <- dmai(d, date = "when", x = c("cur", "dep"), r = c("rcur", "rdep"), benchmark = "bond")
  b <- dmai(d3, benchmark = "rb")
  expect_equal(a$DMAI, b$DMAI)
  expect_identical(names(a)[1L], "when")
})

test_that("errors name the function and the problem", {
  expect_error(dmai(d3, method = "Laspeyres"), "dmai\\(\\): `method`")
  expect_error(dmai(d3[, -1]), "`date`")
  bad <- d3
  bad$x1[2] <- -1
  expect_error(dmai(bad), "positive")
  expect_error(dmai(d3, r = "r1"), "`r`")
  expect_error(dmai(d3, benchmark = "nope"), "no column")
  expect_error(dmai(cpi_ch19, method = "CE", date = "period", x = paste0("p", 1:6), user_cost = paste0("q", 1:6)),
               "rates")
  expect_error(dmai(d3, logbase = 1), "logbase")
})

test_that("the comparison, user costs and moments", {
  cmp <- dmai_compare(money_sim, benchmark = "rb")
  expect_result(cmp, "compare", c("indexes", "summary"))
  expect_identical(levels(cmp$indexes$method), c("Barnett", "Hancock", "Fisher", "SimpleSum", "CE"))
  expect_equal(cmp$indexes[method == "Barnett"]$index, dmai(money_sim, benchmark = "rb")$index)
  expect_warning(c2 <- dmai_compare(cpi_ch19, date = "period", x = paste0("p", 1:6), user_cost = paste0("q", 1:6)),
                 "currency equivalent")
  expect_false("CE" %in% c2$summary$method)
  uc <- dmai_user_cost(money_sim, benchmark = "rb")
  expect_result(uc, "user_cost", c("components", "benchmark"))
  expect_equal(uc$components[, sum(share), by = Date]$V1, rep(1, nrow(money_sim)))
  m <- dmai_moments(money_sim, benchmark = "rb")
  expect_result(m, "moments", "moments")
  f <- dmai(money_sim, benchmark = "rb", as_dt = FALSE)
  expect_equal(dmai_moments(f)$moments, m$moments)
  expect_equal(m$moments$growth[-1], f$index$DMAI[-1])
  expect_true(all(m$moments$variance[-1] >= 0))
})

test_that("plots, interpretations and references", {
  f <- dmai(money_sim, benchmark = "rb", as_dt = FALSE)
  for (w in c("index", "growth", "shares", "user_cost", "moments", "price")) expect_s3_class(plot(f, which = w), "ggplot")
  expect_s3_class(ggplot2::autoplot(f, which = "shares"), "ggplot")
  cmp <- dmai_compare(money_sim, benchmark = "rb")
  expect_s3_class(plot(cmp, which = "growth"), "ggplot")
  for (x in list(f, cmp, dmai_user_cost(money_sim, benchmark = "rb"), dmai_moments(f))) {
    it <- dmai_interpret(x)
    expect_s3_class(it, "dmai_interpretation")
    expect_true(nzchar(it$headline))
    expect_output(print(it))
    expect_match(paste(format(it, style = "markdown"), collapse = "\n"), "^## ")
    expect_s3_class(dmai_cite(x), "bibentry")
  }
  expect_match(dmai_interpret(f)$headline, "simple sum")
  expect_match(paste(dmai_interpret(dmai(money_sim, as_dt = FALSE))$cautions, collapse = " "), "benchmark")
  expect_error(dmai_interpret(dmai(money_sim)), "as_dt = FALSE")
  expect_output(print(dmai_cite()), "Barnett")
  expect_output(print(dmai_cite(style = "bibtex")), "@Article")
})
