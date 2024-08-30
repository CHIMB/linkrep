test_that("Testing display_headers updates the table", {
  data <- data.table(abbrev = c("idk", "fyi"), descr = c("i don't know", "for your information"))
  ft <- two_column_table(data, "docx", display_headers = TRUE)
  expect_equal(length(ft$header$content$data), 2)

  ft <- two_column_table(data, "docx", display_headers = FALSE)
  expect_equal(length(ft$header$content$data), 0)
})
