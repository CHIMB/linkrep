test_that("test header_align and body_align parameter checks", {
  # valid inputs
  data <- data.frame(a = "apple", b = "banana")
  output_format <- "docx"
  font_size <- 12
  font_style <- system_fonts()$family[1]
  expect_no_error(formatted_flextable(data,
                                      output_format,
                                      font_size,
                                      font_style,
                                      header_align = "center"))
  expect_no_error(formatted_flextable(data,
                                      output_format,
                                      font_size,
                                      font_style,
                                      header_align = "left"))
  expect_no_error(formatted_flextable(data,
                                      output_format,
                                      font_size,
                                      font_style,
                                      body_align = "right"))
  expect_no_error(formatted_flextable(data,
                                      output_format,
                                      font_size,
                                      font_style,
                                      header_align = "center",
                                      body_align = "right"))

  # invalid inputs
  expect_error(formatted_flextable(data,
                                   output_format,
                                   font_size,
                                   font_style,
                                   header_align = "middle"),
               "Invalid argument: header_align. Must be one of 'left', 'center', 'right'")
  expect_error(formatted_flextable(data,
                                   output_format,
                                   font_size,
                                   font_style,
                                   header_align = "center",
                                   body_align = "port"),
               "Invalid argument: body_align. Must be one of 'left', 'center', 'right'")
  expect_error(formatted_flextable(data,
                                   output_format,
                                   font_size,
                                   font_style,
                                   header_align = "starboard",
                                   body_align = "left"),
               "Invalid argument: header_align. Must be one of 'left', 'center', 'right'")
  expect_error(formatted_flextable(data,
                                   output_format,
                                   font_size,
                                   font_style,
                                   body_align = "center stage"),
               "Invalid argument: body_align. Must be one of 'left', 'center', 'right'")
})


test_that("Testing the table headers were correctly labelled", {
  out_f <- "pdf"
  f_size <- 12
  f_style <- "Arial"

  data <- data.frame(a = "aa", b = "bb", c = "cc")
  ft <- formatted_flextable(data, out_f, f_size, f_style)
  labels <- ft$header$content$data
  expect_equal(c(labels[[1]][[1]], labels[[2]][[1]], labels[[3]][[1]]), c("a", "b", "c"))

  Hmisc::label(data$a) <- "apple"
  Hmisc::label(data$c) <- "corn"
  ft <- formatted_flextable(data, out_f, f_size, f_style)
  labels <- ft$header$content$data
  expect_equal(c(labels[[1]][[1]], labels[[2]][[1]], labels[[3]][[1]]), c("apple", "b", "corn"))

  Hmisc::label(data$b) <- "banana"
  ft <- formatted_flextable(data, out_f, f_size, f_style)
  labels <- ft$header$content$data
  expect_equal(c(labels[[1]][[1]], labels[[2]][[1]], labels[[3]][[1]]), c("apple", "banana", "corn"))
})


test_that("Testing the fonts were correctly updated", {
  out_f <- "pdf"
  ft <- formatted_flextable(mtcars, out_f, 13, "Arial")

  # check font size
  expect_equal(ft$header$styles$text$font.size$data[[1]], 13)
  expect_equal(ft$body$styles$text$font.size$data[[1]], 13)

  # check font style
  expect_equal(ft$header$styles$text$font.family$data[[1]], "Arial")
  expect_equal(ft$body$styles$text$font.family$data[[1]], "Arial")

  ft <- formatted_flextable(mtcars, out_f, 22, "Calibri", "this is my footnote!")

  # check font size
  expect_equal(ft$header$styles$text$font.size$data[[1]], 22)
  expect_equal(ft$body$styles$text$font.size$data[[1]], 22)
  expect_equal(ft$footer$styles$text$font.size$data[[1]], 22)

  # check font style
  expect_equal(ft$header$styles$text$font.family$data[[1]], "Calibri")
  expect_equal(ft$body$styles$text$font.family$data[[1]], "Calibri")
  expect_equal(ft$footer$styles$text$font.family$data[[1]], "Calibri")

})


test_that("Testing the footnotes were correctly updated", {
  out_f <- "pdf"

  ft <- formatted_flextable(mtcars, out_f, 12, "Arial", NULL)
  expect_null(ft$footer$content$data[1][[1]])

  ft <- formatted_flextable(mtcars, out_f, 12, "Calibri", "this is my footnote!")
  expect_equal(ft$footer$content$data[[1]]$txt, "this is my footnote!")

  ft <- formatted_flextable(mtcars, out_f, 12, "Calibri", c("uno", "dos", "tres"))
  expect_equal(ft$footer$content$data[[1]]$txt, "uno\ndos\ntres")
})




test_that("Testing the text alignments were correctly updated", {
  out_f <- "pdf"
  ft <- formatted_flextable(mtcars, out_f, 13, "Arial", header_align = "left", body_align = "right")

  expect_equal(ft$header$styles$pars$text.align$data[[1]], "left")
  expect_equal(ft$body$styles$pars$text.align$data[[1]], "right")

  ft <- formatted_flextable(mtcars, out_f, 13, "Arial", header_align = "center")
  expect_equal(ft$header$styles$pars$text.align$data[[1]], "center")

  ft <- formatted_flextable(mtcars, out_f, 13, "Arial", body_align = "left")
  expect_equal(ft$body$styles$pars$text.align$data[[1]], "left")
})
