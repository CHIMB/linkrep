test_that("Testing footnotes output correctly", {
  data <- data.frame(a = c(0,1,1,1,1,0), b = c(0,1,0,1,0,1))
  out_f <- "docx"
  mt <- suppressMessages(missingness_table(data, out_f))
  expect_equal(mt$footer$content$data[[1]]$txt, "Data are presented as n (%).")

  mt <- suppressMessages(missingness_table(data, out_f, footnotes = c("this my first", "this my second")))
  expect_equal(mt$footer$content$data[[1]]$txt,
               "this my first\nthis my second\nData are presented as n (%).")
})
