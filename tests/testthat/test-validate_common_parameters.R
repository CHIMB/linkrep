test_that("testing extra conditions for output_format parameter", {
  # valid inputs
  expect_no_error(validate_common_parameters(output_format = "pdf"))
  expect_no_error(validate_common_parameters(output_format = "docx"))

  # edge cases
  expect_error(validate_common_parameters(output_format = "not correct"),
               "Invalid argument: output_format. Options: 'pdf' or 'docx'")
  expect_error(validate_common_parameters(output_format = "pdff"),
               "Invalid argument: output_format. Options: 'pdf' or 'docx'")
  expect_error(validate_common_parameters(output_format = "docxdocx"),
               "Invalid argument: output_format. Options: 'pdf' or 'docx'")
  })

test_that("testing extra conditions for num_decimal_places parameter", {
  # valid inputs
  expect_no_error(validate_common_parameters(num_decimal_places = 0))
  expect_no_error(validate_common_parameters(num_decimal_places = 10.0))

  # edge cases
  expect_error(validate_common_parameters(num_decimal_places = -1),
               "Invalid argument: num_decimal_places must be >= 0.")
  expect_error(validate_common_parameters(num_decimal_places = -3),
               "Invalid argument: num_decimal_places must be >= 0.")
})

test_that("testing extra conditions for font_size parameter", {
  # valid inputs
  expect_no_error(validate_common_parameters(font_size = 1))
  expect_no_error(validate_common_parameters(font_size = 50))

  # edge cases
  expect_error(validate_common_parameters(font_size = 0),
               "Invalid argument: font_size. font_size must be > 0.")
  expect_error(validate_common_parameters(font_size = -45),
               "Invalid argument: font_size. font_size must be > 0.")
})

test_that("testing extra conditions for font_style parameter", {
  # valid inputs
  expect_no_error(validate_common_parameters(font_style = system_fonts()$family[3]))
  expect_no_error(validate_common_parameters(font_style = system_fonts()$name[15]))

  # edge cases
  expect_error(validate_common_parameters(font_style = "fonty1"))
  expect_error(validate_common_parameters(font_style = "i hope this isn't the name of a font"))
})
