test_that("test width of table remains the same", {
  ft <- flextable(data.frame(a = "a", b = "b"))
  expect_equal(flextable_dim(set_table_width(ft))$widths, flextable_dim(autofit(ft))$widths)

  ft <- flextable(data.frame(One = c(1, 11, 111), Two = c(2, 22, 222), Three = c(3, 33, 333)))
  expect_equal(flextable_dim(set_table_width(ft))$widths, flextable_dim(autofit(ft))$widths)
})

test_that("test width of table shrinks to fit page", {
  ft <- flextable(data.frame("my first variable" = 1,
                             "my second variable" = 2,
                             "my third variable" = 3,
                             "my fourth variable" = 4,
                             "my fifth variable" = 5,
                             "my sixth variable" = 6,
                             "my seventh variable" = 7,
                             "my eighth variable" = 8,
                             "my ninth variable" = 9,
                             "my tenth variable" = 10))

  # would need to change this if PORTRAIT_WIDTH, LANDSCAPE_WIDTH or MIN_COL_WIDTH change within function
  expect_equal(flextable_dim(set_table_width(ft))$widths, 8.5)
})
