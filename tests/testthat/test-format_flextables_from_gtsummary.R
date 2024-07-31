test_that("Testing table modifications are applied correctly", {
  ft <- flextable(mtcars)
  out_f <- "pdf"
  ft_new <- format_flextables_from_gtsummary(ft, out_f, 13, "Arial", NULL)

  # check footnote
  expect_null(ft_new$footer$content$data[1][[1]])

  # check font size
  expect_equal(ft_new$header$styles$text$font.size$data[[1]], 13)
  expect_equal(ft_new$body$styles$text$font.size$data[[1]], 13)

  # check font style
  expect_equal(ft_new$header$styles$text$font.family$data[[1]], "Arial")
  expect_equal(ft_new$body$styles$text$font.family$data[[1]], "Arial")

  ft_new <- format_flextables_from_gtsummary(ft, out_f, 22, "Calibri", "this is my footnote!")

  # check footnote
  expect_equal(ft_new$footer$content$data[[1]]$txt, "this is my footnote!")

  # check font size
  expect_equal(ft_new$header$styles$text$font.size$data[[1]], 22)
  expect_equal(ft_new$body$styles$text$font.size$data[[1]], 22)
  expect_equal(ft_new$footer$styles$text$font.size$data[[1]], 22)

  # check font style
  expect_equal(ft_new$header$styles$text$font.family$data[[1]], "Calibri")
  expect_equal(ft_new$body$styles$text$font.family$data[[1]], "Calibri")
  expect_equal(ft_new$footer$styles$text$font.family$data[[1]], "Calibri")

})
