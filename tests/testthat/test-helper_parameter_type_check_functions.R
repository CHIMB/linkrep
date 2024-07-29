test_that("boolean helper function validates logical parameter", {
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

test_that("string helper function validates string parameter", {
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

test_that("numeric helper function validates numeric parameter", {
  # valid inputs
  expect_no_error(validate_number(333, "n"))
  expect_no_error(validate_number(-5.8, "n"))
  expect_no_error(validate_number(as.integer(10), "n"))
  expect_no_error(validate_number(90L, "n"))

  # invalid inputs
  expect_error(validate_number(c(2,3,4), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number("-11", "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(FALSE, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(NA, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(NULL, "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(list(s = c(9,0,2,1)), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(data.frame(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(factor(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(data.table(), "n"),
               "Invalid argument: n. n must be a single numeric value")
  expect_error(validate_number(c(), "n"),
               "Invalid argument: n. n must be a single numeric value")
})

test_that("string vector helper function validates string vector parameter", {
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

test_that("dataframe helper function validates dataframe parameter", {
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

test_that("var in dataframe helper function validates variable parameter is in the data parameter", {
  # valid inputs
  expect_no_error(validate_var_in_data("Species", iris, "type", "data"))
  expect_no_error(validate_var_in_data("air", data.table(air = 22), "type", "data"))

  # invalid inputs
  expect_error(validate_var_in_data("var", mtcars, "type", "data"))
  expect_error(validate_var_in_data("eyes", iris, "type", "data"))
})

test_that("binary/logical dataframe helper function validates binary/logical dataframe parameter", {
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
