test_that("Testing factoring to output 'Linked' and 'Unlinked' works with both binary and logical variables", {
  out_f <- "docx"

  # check binary variable
  linked <- sample(0:1, nrow(CO2), replace = TRUE)
  main_data <- cbind(CO2, linked)
  t <- suppressMessages(linkage_rate_table(main_data, out_f, "linked", names(CO2)))
  expect_equal(t$header$content$data[[2]]$txt[1], "Linked")
  expect_equal(t$header$content$data[[3]]$txt[1], "Unlinked")

  t <- suppressMessages(linkage_rate_table(main_data, out_f, "linked", names(CO2),
                          display_total_column = F, display_mean_not_median_stats = T,
                          display_percent_symbol = T))
  expect_equal(t$header$content$data[[2]]$txt[1], "Linked")
  expect_equal(t$header$content$data[[3]]$txt[1], "Unlinked")

  # check logical variable
  linked <- sample(c(T,F), nrow(mtcars), replace = TRUE)
  main_data <- cbind(mtcars, linked)
  t <- suppressMessages(linkage_rate_table(main_data, out_f, "linked", names(mtcars)))
  expect_equal(t$header$content$data[[2]]$txt[1], "Linked")
  expect_equal(t$header$content$data[[3]]$txt[1], "Unlinked")

  t <- suppressMessages(linkage_rate_table(main_data, out_f, "linked", names(mtcars),
                          display_total_column = F, display_mean_not_median_stats = T,
                          display_percent_symbol = T))
  expect_equal(t$header$content$data[[2]]$txt[1], "Linked")
  expect_equal(t$header$content$data[[3]]$txt[1], "Unlinked")
})


# data to be used throughout?
linked <- sample(0:1, nrow(CO2), replace = TRUE)
main_data <- cbind(CO2, linked)
out_f <- "docx"
col_var <- "linked"
strata_vars <- names(CO2)

test_that("Testing display_total_column performs correctly", {
  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var, strata_vars))
  expect_equal(length(t$col_keys), 4)

  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var, strata_vars,
                                           display_total_column = FALSE))
  expect_equal(length(t$col_keys), 3)
})

test_that("Testing the missing indicators dataset is correctly added to the table", {
  Hmisc::label(main_data[[2]]) <- "Plant Origin"
  Hmisc::label(main_data[[5]]) <- "Uptake"
  main_data_missing <- data.frame(var1 = sample(0:1, nrow(main_data), TRUE),
                                  var2 = sample(c(T, F), nrow(main_data), TRUE),
                                  var3 = sample(c(T,F), nrow(main_data), TRUE),
                                  var4 = sample(0:1, nrow(main_data), TRUE),
                                  var5 = sample(0:1, nrow(main_data), TRUE),
                                  var6 = sample(c(T,F), nrow(main_data), TRUE))
  colnames(main_data_missing) <- c(paste0(names(CO2), "_missing")[1], # tests matching on varname_missing
                                   paste0(names(CO2), "_missing")[4],
                                   "ut",
                                   "p_orig",
                                   "stuff", # tests extra variables get added to the end of the table
                                   "oxygen_missing")
  Hmisc::label(main_data_missing[[3]]) <- "Uptake" # tests matching on labels
  Hmisc::label(main_data_missing[[4]]) <- "Plant Origin"

  t <- suppressMessages(linkage_rate_table(main_data, "pdf", col_var, strata_vars,
                                           main_data_missing))
  # check all the rows in the table have been inserted correctly
  expect_equal(t$body$dataset$label,
               c("Plant", "\tQn1","\tQn2", "\tQn3", "\tQc1", "\tQc3",
                  "\tQc2", "\tMn3","\tMn2", "\tMn1","\tMc2", "\tMc3",
                  "\tMc1", "\tMissing",
                  "Plant Origin", "\tQuebec", "\tMississippi", "\tMissing",
                  "Treatment", "\tnonchilled","\tchilled",
                  "conc", "\t95", "\t175", "\t250", "\t350", "\t500", "\t675",
                  "\t1000", "\tMissing",
                  "Uptake", "\tMissing",
                  "Missing stuff",
                  "Missing oxygen_missing"))


})

test_that("Testing table outputs with or without main_data_missing...", {
  expect_s3_class(suppressMessages(linkage_rate_table(main_data, out_f, col_var, strata_vars)),
                  "flextable")
  data_mis <- data.frame(var = sample(0:1, nrow(main_data), T),
                         var2 = sample(c(T,F), nrow(main_data), T))
  colnames(data_mis) <- c(paste0(names(CO2), "_missing")[1], "fun")
  expect_s3_class(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                      strata_vars, data_mis)),
                  "flextable")
})

test_that("Testing parameters with extra type checks", {
  # strata_vars:
  # invalid inputs
  expect_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                      c("var uno"))),
               "Invalid argument: strata_vars. Not all variables provided in strata_vars are present in 'main_data'")
  expect_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                      c("one two three", "not only", "you and me"))),
               "Invalid argument: strata_vars. Not all variables provided in strata_vars are present in 'main_data'")


  # output_dir:
  # valid input
  # system.file() won't work if package is not installed
  expect_no_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                      strata_vars,
                                                      output_dir = system.file("R", package = "linkrep"))))

  # invalid inputs
  # system.file() won't work if package is not installed
  expect_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                   strata_vars,
                                                   output_dir = system.file("README.Rmd", package = "linkrep"))))
  expect_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                   strata_vars,
                                                   output_dir = "directory")))

  # check error is thrown when output_to_csv = T and output_dir is not provided
  expect_error(suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                                   strata_vars, output_to_csv = T)),
               "output_dir must be provided when output_to_csv is TRUE")
})

test_that("Testing csv output option", {
  # system.file() won't work if package is not installed
  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                           strata_vars, output_to_csv = T,
                                           output_dir = system.file("tests", package = "linkrep")))
  out_file <- paste0(system.file("tests", package = "linkrep"), "/linkage_rate_table.csv")
  expect_true(file.exists(out_file))
  unlink(out_file)
})

test_that("Testing 0(NA) does not show up in table", {
  data_missing <- data.frame(var = rep(0, nrow(main_data)),
                             var2 = rep(F, nrow(main_data)))

  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                           strata_vars, data_missing))
  expect_equal(t$body$content$data[[59]]$txt, "0 (0.0)")
  expect_equal(t$body$content$data[[60]]$txt, "0 (0.0)")
  expect_equal(t$body$content$data[[89]]$txt, "0 (0.0)")
  expect_equal(t$body$content$data[[90]]$txt, "0 (0.0)")

  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var,
                                           strata_vars, data_missing,
                                           decimal_mark = "_",
                                           num_decimal_places = 3))
  expect_equal(t$body$content$data[[59]]$txt, "0 (0_000)")
  expect_equal(t$body$content$data[[60]]$txt, "0 (0_000)")
  expect_equal(t$body$content$data[[89]]$txt, "0 (0_000)")
  expect_equal(t$body$content$data[[90]]$txt, "0 (0_000)")
})

test_that("Testing footnotes output correctly", {
  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var, strata_vars))
  expect_equal(t$footer$content$data[[1]]$txt, 'Data are presented as n (%), mean \u00B1 SD, or median (Q1, Q3); where SD = standard deviation, Q1 = 25\u1d57\u02b0 percentile and Q3 = 75\u1d57\u02b0 percentile.')

  t <- suppressMessages(linkage_rate_table(main_data, out_f, col_var, strata_vars,
                                           footnotes = c("one", "un", "uno", "eins")))
  expect_equal(t$footer$content$data[[1]]$txt, "one\nun\nuno\neins\nData are presented as n (%), mean \u00B1 SD, or median (Q1, Q3); where SD = standard deviation, Q1 = 25\u1d57\u02b0 percentile and Q3 = 75\u1d57\u02b0 percentile.")
})



# source("C:/Users/stoughte/Documents/CHI Github/BMD Data/making_data_report_ready.R")
#
# t <- linkage_rate_table(bmd_data,
#                    "docx",
#                    "linked",
#                    c("gender", "acquisition_year_categ", "health_centre"),
#                    bmd_data_missing)
#
# set.seed(55)
# linked <- sample(0:1, nrow(mtcars), replace = TRUE)
# data <- cbind(mtcars, linked)
# linkage_rate_table(data, "docx", "linked", names(mtcars), display_mean_not_median_stats = TRUE)

# check factoring worked (so try with binary and logical variables)
# Linked/Unlinked

# check remove total column

# check with and without missing dataset

# check csv outputs if requested

# check all the extra type checks:
# strata_vars, output_dir, if output_to_csv T and output_dir NULL, ...

# check 0(0) outputs instead of 0(NA)

# check Missing becomes subset of main var

# check that the above is done correctly, with label and var_missing

# check the continuous stats



