test_that("Testing validate_boolean function", {
  # valid inputs
  expect_no_error(validate_boolean(TRUE, "b"))
  expect_no_error(validate_boolean(FALSE, "b"))

  # invalid inputs
  expect_error(validate_boolean("TRUE", "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(c(T,F,T), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(22, "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(NA, "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(NULL, "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(list(a = "a"), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(data.frame(), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(factor(), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(data.table(x = TRUE), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
  expect_error(validate_boolean(c(), "b"),
               "Invalid argument: b. b must be TRUE or FALSE")
})

test_that("Testing validate_string function", {
  # valid input
  expect_no_error(validate_string("hello", "s"))

  # invalid inputs
  expect_error(validate_string(c("str1", "str2", "str3"), "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(-100, "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(FALSE, "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(NA, "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(NULL, "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(list(), "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(data.frame(a = c(T,F)), "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(factor(), "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(data.table(), "s"),
               "Invalid argument: s. s must be a single character string")
  expect_error(validate_string(c(), "s"),
               "Invalid argument: s. s must be a single character string")
})

test_that("Testing validate_string_vector function", {
  # valid inputs
  expect_no_error(validate_string_vector("me", "v"))
  expect_no_error(validate_string_vector(c("me"), "v"))
  expect_no_error(validate_string_vector(c("me and you", "you and I"), "v"))

  # invalid inputs
  expect_error(validate_string_vector(100000, "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(F, "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(NA, "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(NULL, "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(list(a = "abba", b = "beetles"), "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(data.frame(str = "string"), "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(factor(), "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(data.table(table = 2), "v"),
               "Invalid argument: v. v must be a character string vector")
  expect_error(validate_string_vector(c(), "v"),
               "Invalid argument: v. v must be a character string vector")
})

test_that("Testing validate_numeric function", {
  # valid inputs
  expect_no_error(validate_numeric(333, "n"))
  expect_no_error(validate_numeric(-5.8, "n"))
  expect_no_error(validate_numeric(as.integer(10), "n"))
  expect_no_error(validate_numeric(90L, "n"))

  # invalid inputs
  expect_error(validate_numeric(c(2,3,4), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric("-11", "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(FALSE, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(NA, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(NULL, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(list(s = c(9,0,2,1)), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(data.frame(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(factor(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(data.table(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_numeric(c(), "n"),
               "Invalid argument: n. n must be a single numeric value")
})

test_that("Testing validate_numeric_vector function", {
  # valid inputs
  expect_no_error(validate_numeric_vector(333, "n"))
  expect_no_error(validate_numeric_vector(as.integer(-5.8), "n"))
  expect_no_error(validate_numeric_vector(90L, "n"))
  expect_no_error(validate_numeric_vector(c(-11, 50L, 0, 981), "n"))

  # invalid inputs
  expect_error(validate_numeric_vector("-11", "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(FALSE, "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(NA, "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(NULL, "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(list(s = c(9,0,2,1)), "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(data.frame(), "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(factor(), "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(data.table(), "n"),
               "Invalid argument: n. n must be a numeric vector")
  expect_error(validate_numeric_vector(c(), "n"),
               "Invalid argument: n. n must be a numeric vector")
})

test_that("Testing validate_df function", {
  # valid inputs
  expect_no_error(validate_df(data.frame(var1 = "var", var2 = TRUE, var3 = 77), "d"))
  expect_no_error(validate_df(data.table(a = c(9,3,2,1)), "d"))
  expect_no_error(validate_df(dplyr::tibble(fruit = c("banana", "apple"), veggies = "carrot", "pepper"), "d"))
  expect_no_error(validate_df(mtcars, "d"))

  # invalid inputs
  expect_error(validate_df(-10, "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(c(TRUE, 90), "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df("data", "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(NA, "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(NULL, "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(list(data = data.frame(a = "a")), "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(factor(), "d"),
               "Invalid argument: d. d must be a dataframe")
  expect_error(validate_df(matrix(), "d"),
               "Invalid argument: d. d must be a dataframe")

  # edge cases
  expect_error(validate_df(data.frame(), "d"),
               "Invalid argument: d. d cannot be empty")
  expect_error(validate_df(data.table(), "d"),
               "Invalid argument: d. d cannot be empty")
  expect_error(validate_df(dplyr::tibble(), "d"),
               "Invalid argument: d. d cannot be empty")
})

test_that("Testing validate_var_in_data function", {
  # valid inputs
  expect_no_error(validate_var_in_data("Species", iris, "type", "data"))
  expect_no_error(validate_var_in_data("air", data.table(air = 22), "type", "data"))

  # invalid inputs
  expect_error(validate_var_in_data("var", mtcars, "type", "data"))
  expect_error(validate_var_in_data("eyes", iris, "type", "data"))
})

test_that("Testing validate_df_binary function", {
  # valid inputs
  expect_no_error(validate_df_binary(data.frame(bin = c(0,1,0,0,0,1)), "bl"))
  expect_no_error(validate_df_binary(data.frame(a = 1, b = 0), "bl"))
  expect_no_error(validate_df_binary(data.table(var = c(T,F), lol = c(1,1)), "bl"))
  expect_no_error(validate_df_binary(data.table(mixed = c(0,1,TRUE, 0, FALSE)), "bl"))

  # invalid inputs
  expect_error(validate_df_binary(data.frame(var = c("hi", TRUE, 0)), "bl"),
               "Invalid argument: bl. All variables must be binary or logical")
  expect_error(validate_df_binary(data.table(w = 1, u = 9, b = 12), "bl"),
               "Invalid argument: bl. All variables must be binary or logical")
  expect_error(validate_df_binary(data.frame(bin = c(TRUE, FALSE), bin2 = c(0,1), not = c("hi", 0)), "bl"),
               "Invalid argument: bl. All variables must be binary or logical")
})


test_that("Testing validate_flextable function", {
  # valid inputs
  expect_no_error(validate_flextable(flextable(mtcars), "tbl"))

  # invalid inputs
  expect_error(validate_flextable(mtcars, "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable("flextable", "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(39, "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(FALSE, "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(gtsummary::tbl_summary(iris), "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(NA, "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(NULL, "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(list(tbl = list()), "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
  expect_error(validate_flextable(factor(), "tbl"),
               "Invalid argument: tbl. tbl must be a flextable object")
})


test_that("Testing extra conditions for validate_common_parameters function", {
  # output_format parameter checks:
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

  # num_decimal_places parameter checks:
  # valid inputs
  expect_no_error(validate_common_parameters(num_decimal_places = 0))
  expect_no_error(validate_common_parameters(num_decimal_places = 10.0))

  # edge cases
  expect_error(validate_common_parameters(num_decimal_places = -1),
               "Invalid argument: num_decimal_places must be >= 0.")
  expect_error(validate_common_parameters(num_decimal_places = -3),
               "Invalid argument: num_decimal_places must be >= 0.")

  # font_size parameter checks:
  # valid inputs
  expect_no_error(validate_common_parameters(font_size = 1))
  expect_no_error(validate_common_parameters(font_size = 50))

  # edge cases
  expect_error(validate_common_parameters(font_size = 0),
               "Invalid argument: font_size. font_size must be > 0.")
  expect_error(validate_common_parameters(font_size = -45),
               "Invalid argument: font_size. font_size must be > 0.")

  # font_style parameter checks:
  # valid inputs
  expect_no_error(validate_common_parameters(font_style = system_fonts()$family[3]))
  expect_no_error(validate_common_parameters(font_style = system_fonts()$name[15]))

  # edge cases
  expect_error(validate_common_parameters(font_style = "fonty1"))
  expect_error(validate_common_parameters(font_style = "i hope this isn't the name of a font"))
})
