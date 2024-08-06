report_title <- "Title"
left_ds_name <- "Left"
right_ds_name <- "Right"
out_dir <- tempdir() #UNLINK THIS WHEN DONE USINGGGG!!!!!
linker <- "Me"
lr_tbl_col_var <- "c"
lr_tbl_strata_vars <- "s"

test_that("Testing data input checks work", {
  expect_error(linkage_quality_report(26, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. main_data must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report(TRUE, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. main_data must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report("data", report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. File not found")
  expect_error(linkage_quality_report("data.rds", report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. File not found")
  expect_error(linkage_quality_report("data.csv", report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. File not found")
  expect_error(linkage_quality_report(tempdir(), report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. File extension must be either .rds or .csv")
  expect_error(linkage_quality_report(list(v = 2, b = 5), report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. main_data must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report(factor(), report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. main_data must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report(c("back", "streets", "back", "alright!"), report_title,
                                    left_ds_name, right_ds_name, out_dir, linker,
                                    lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: main_data. Must be single character string")
  expect_error(linkage_quality_report(c(), report_title, left_ds_name, right_ds_name,
                                    out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars),
             "Invalid argument: main_data. main_data must be a dataframe or a file path to a csv or rds file")

  expect_error(linkage_quality_report(mtcars, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      missing_data_indicators = "missing data"),
               "Invalid argument: missing_data_indicators. File not found")
  expect_error(linkage_quality_report(mtcars, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      missing_data_indicators = CO2,
                                      abbreviations = list(a = "apple", b = "blueberry")),
               "Invalid argument: abbreviations. abbreviations must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report(mtcars, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      missing_data_indicators = CO2,
                                      abbreviations = iris,
                                      algorithm_summary_data = factor()),
               "Invalid argument: algorithm_summary_data. algorithm_summary_data must be a dataframe or a file path to a csv or rds file")
  expect_error(linkage_quality_report(mtcars, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      missing_data_indicators = CO2,
                                      abbreviations = iris,
                                      algorithm_summary_data = airquality,
                                      performance_measures_data = "clas_metrs.csv"),
               "Must provide ground_truth and ground_truth_missing_var with 'performance_measures_data'")

})

main_data <- mtcars
test_that("Testing input checks for parameters with extra conditions", {
  # temp_data_output_dir:
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      temp_data_output_dir = "out"),
               "Invalid argument: temp_data_output_dir. temp_data_output_dir must be a path to a directory")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      temp_data_output_dir = tempfile()),
               "Invalid argument: temp_data_output_dir. temp_data_output_dir must be a path to a directory")

  # performance measures provided without ground truth
  pm_data <- CO2
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      performance_measures_data = pm_data),
               "Must provide ground_truth and ground_truth_missing_var with 'performance_measures_data'")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      performance_measures_data = pm_data,
                                      ground_truth = "PHIN"),
               "Must provide ground_truth and ground_truth_missing_var with 'performance_measures_data'")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name, right_ds_name,
                                      out_dir, linker, lr_tbl_col_var, lr_tbl_strata_vars,
                                      performance_measures_data = pm_data,
                                      ground_truth_missing_var = "PHIN_missing"),
               "Must provide ground_truth and ground_truth_missing_var with 'performance_measures_data'")

  # output_dir
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, "out", linker,
                                      lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: output_dir. output_dir must be a path to a directory")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, tempfile(), linker,
                                      lr_tbl_col_var, lr_tbl_strata_vars),
               "Invalid argument: output_dir. output_dir must be a path to a directory")

  # num_records_right_dataset
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, num_records_right_dataset = -40),
               "Invalid argument: num_records_right_dataset. num_records_right_dataset must be > 0")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, num_records_right_dataset = 0),
               "Invalid argument: num_records_right_dataset. num_records_right_dataset must be > 0")

  # acquisition_month_var provided without acquisition_year_var
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, acquisition_month_var = "month"),
               "'acquisition_year_var' must be provided with 'acquisition_month_var'")



})


test_that("Testing input checks for background image parameters", {
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, cover_page = "cover"),
               "Invalid argument: cover_page. File not found")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, cover_page = tempdir()),
               "Invalid argument: cover_page. File extension must be either .png, .pdf, .jpg or .jpeg")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, content_portrait_page = "page"),
               "Invalid argument: content_portrait_page. File not found")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, content_portrait_page = tempdir()),
               "Invalid argument: content_portrait_page. File extension must be either .png, .pdf, .jpg or .jpeg")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, content_landscape_page = "land"),
               "Invalid argument: content_landscape_page. File not found")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, content_landscape_page = tempdir()),
               "Invalid argument: content_landscape_page. File extension must be either .png, .pdf, .jpg or .jpeg")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, back_cover_page = "backpage"),
               "Invalid argument: back_cover_page. File not found")
  expect_error(linkage_quality_report(main_data, report_title, left_ds_name,
                                      right_ds_name, out_dir, linker, lr_tbl_col_var,
                                      lr_tbl_strata_vars, back_cover_page = tempdir()),
               "Invalid argument: back_cover_page. File extension must be either .png, .pdf, .jpg or .jpeg")

})


test_that("Testing checks for subsituting values into other files", {


})

