# Regression against version 0.5.0: its example (dev/legacy/examples/dmai.R, with a seed) is run with this version
# and DMAI is compared with the saved results. 0.5.0 used logarithms to base 10 by default; 1.0.0 uses natural
# logarithms (a correction, dev/legacy/exceptions.csv), and logbase = 10 reproduces the old default.
test_that("dmai() reproduces version 0.5.0", {
  dir <- legacy_dir()
  skip_if(!nzchar(dir), "no dev/legacy snapshots (source tree only)")
  old <- readRDS(file.path(dir, "dmai.rds"))$objects
  D <- old$Data
  check <- function(new, ref, label) {
    expect_equal(as.Date(new$Date), as.Date(ref$Date), label = paste(label, "dates"))
    expect_equal(new$DMAI, ref$DMAI, tolerance = 1e-10, label = paste(label, "DMAI"))
  }
  check(dmai(.data = D, method = "Barnett", logbase = 10), old$DMAIBarnett, "default of 0.5.0 (base 10)")
  check(dmai(.data = D, method = "Barnett", logbase = 10), old$DMAIBarnett1, "logbase = 10")
  check(dmai(.data = D, method = "Barnett", logbase = 2), old$DMAIBarnett2, "logbase = 2")
  check(dmai(.data = D, method = "Barnett", logbase = exp(1)), old$DMAIBarnett3, "logbase = e")
  check(dmai(.data = D, method = "Hancock"), old$DMAIHancock, "Hancock")
  # the corrected default: natural logarithms, ln(10) times the old default
  new <- dmai(.data = D, method = "Barnett")
  expect_equal(new$DMAI, old$DMAIBarnett$DMAI * log(10), tolerance = 1e-10)
  expect_equal(new$DMAI, old$DMAIBarnett3$DMAI, tolerance = 1e-10)
})
