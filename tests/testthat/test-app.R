test_that("the application runs every page", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  shiny::testServer(dmai_app_object(), {
    session$setInputs(source = "example", date = "Date", x = paste0("x", 1:8), r = paste0("r", 1:8), bench = "rb",
                      percent = TRUE, method = "Barnett", base = 100, logbase = "e", fit_plot = "index",
                      methods = c("Barnett", "Fisher", "SimpleSum", "CE"), cmp_plot = "index", uc_plot = "shares")
    session$setInputs(run = 1)
    expect_s3_class(fit(), "dmai_index")
    expect_match(.dmai_app_code(input), "dmai\\(")
    session$setInputs(method = "Fisher", run = 2)
    expect_identical(fit()$settings$method, "Fisher")
    session$setInputs(compare = 1)
    expect_s3_class(cmp(), "dmai_compare")
    expect_s3_class(uc(), "dmai_user_cost")
    expect_s3_class(mom(), "dmai_moments")
  })
  code <- .dmai_app_code(list(source = "example", method = "Barnett", logbase = "e", date = "Date",
                              x = paste0("x", 1:8), r = paste0("r", 1:8), bench = "rb", percent = TRUE, base = 100))
  fit <- eval(parse(text = sub("library\\(dmai\\)\n", "", sub("fit\nplot.*$", "", code))))
  expect_equal(fit$index$index, dmai(money_sim, benchmark = "rb")$index)
})
