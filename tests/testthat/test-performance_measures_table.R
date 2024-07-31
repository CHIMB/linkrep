test_that("Testing footnotes are output correctly", {
  ft <- performance_measures_table(mtcars, "docx")
  expect_equal(ft$footer$content$data[[1]]$txt,
               "Data are presented as percentages (%).")

  ft <- performance_measures_table(mtcars, "docx", footnotes = "foot")
  expect_equal(ft$footer$content$data[[1]]$txt,
               "foot\nData are presented as percentages (%).")

  ft <- performance_measures_table(mtcars, "docx", footnotes = c("Wannabe", "Spice Girls"))
  expect_equal(ft$footer$content$data[[1]]$txt,
               "Wannabe\nSpice Girls\nData are presented as percentages (%).")
})
