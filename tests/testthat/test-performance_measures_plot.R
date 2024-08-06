test_that("Testing data checks work", {
  # valid inputs
  expect_no_warning(performance_measures_plot(data.frame(one = 1.3, two = 2L, three = 44)))
  expect_no_warning(performance_measures_plot(data.table(one = 40L, two = 12L, three = 87L)))
  expect_no_warning(performance_measures_plot(data.frame(one = 1.3, two = 99, three = 22)))

  # edge cases
  expect_warning(performance_measures_plot(mtcars),
                 "performance measures is more than one row therefore radar chart will not be output")
  expect_null(suppressWarnings(performance_measures_plot(mtcars)))

  expect_warning(performance_measures_plot(data.frame(num = -100, bum = 70)),
                 "must be more than 2 variables present in performance measures to create radar chart therefore, radar chart will not be output")
  expect_null(suppressWarnings(performance_measures_plot(data.frame(num = -100, bum = 70))))

  expect_warning(performance_measures_plot(data.frame(a = 9, b = "b", c = 2)),
                 "performance measures are not all numeric therefore radar chart will not be output")
  expect_null(suppressWarnings(performance_measures_plot(data.frame(a = 9, b = "b", c = 2))))

  expect_warning(performance_measures_plot(data.frame(num = -100, bum = 70, sum = 999)),
                 "performance measures are not between 0 and 100 therefore radar chart will not output")
  expect_null(suppressWarnings(performance_measures_plot(data.frame(num = -100, bum = 70, sum = 999))))
})




