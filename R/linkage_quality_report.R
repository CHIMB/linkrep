#' Generate a Record Linkage Quality Report
#'
#' The \code{linkage_quality_report} function...
#'
#' @param main_data A data frame, a file path to an rds file that contains a data
#'  frame or a file path to a csv file. This data contains variables present
#'  in the left dataset of the linkage.
#' @param report_title String indicating the title of the report.
#' @param left_dataset_name String indicating the name of the left dataset.
#' @param right_dataset_name String indicating the name of the right dataset.
#' @param output_dir A path to a directory. All output files will be save here.
#' @param data_linker String indicating who performed the linkage.
#' @param linkage_rate_tbl_column_var A string of the name of a logical or binary
#'  variable present in \code{main_data} that indicates whether a record linked or not.
#'  Its values will be the columns in the linkage rate table.
#' @param linkage_rate_tbl_strata_vars A character vector of the names of the
#'  variables present in \code{main_data} to stratify the linkage rate table by.
#' @param linkage_rate_tbl_footnotes A character vector of additional footnotes for
#' the linkage rate table. Each element in the vector will be displayed on a new line.
#' @param linkage_rate_tbl_display_total_column A logical indicating whether to
#'  display a total (overall) column in the table. Default is \code{TRUE}.
#' @param linkage_rate_tbl_display_mean_not_median_stats A logical indicating whether
#'  to display the statistics for continuous variables in the linkage rate table
#'  as either mean \eqn{\pm} standard deviation or median (Q1, Q3), where Q1 is
#'  the 25\eqn{^{th}} percentile, and Q3 is the 75\eqn{^{th}} percentile. Default is \code{FALSE}.
#' @param linkage_rate_tbl_display_alphabetically a
#' @param linkage_rate_tbl_output_to_csv A logical indicating whether to save the
#'  linkage rate table in a csv file. Default is \code{FALSE}.
#' @param missing_data_indicators A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. All variables in the data
#'  must be logical or binary, with \code{1} or \code{TRUE} representing a missing record
#'  for that variable. See Details section for more information on naming
#'  conventions and where this data is used in the report.
#' @param missingness_tbl_footnotes A character vector of additional footnotes for
#' the missingness table. Each element in the vector will be displayed on a new line.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param save_linkage_rate A logical indicating whether to save information on
#'  the linkage in an SQLite file. See Details section for details on what is
#'  saved in the file.
#' @param project_id String indicating the project ID.
#' @param num_records_right_dataset The number of records in the right dataset of the linkage
#' @param acquisition_year_var A string of the name of the numeric variable in
#'  \code{main_data} that represents the acquisition year.
#' @param acquisition_month_var A string of the name of the numeric variable in
#'  \code{main_data} that represents the acquisition month.
#' @param algorithm_summary_data A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. This data contains information
#'  on the linkage algorithm (ex. linking variables on each pass).
#' @param algorithm_summary_tbl_footnotes A character vector of additional footnotes for
#' the algorithm summary table. Each element in the vector will be displayed on a new line.
#' @param performance_measures_data A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. This data contains performance
#'  measures (ex. classification metrics such as sensitivity/recall). Data must be
#'  in percentages with values between 0 and 100 when using the default quarto template.
#' @param performance_measures_tbl_footnotes A character vector of additional footnotes for
#' the performance measures table. Each element in the vector will be displayed on a new line.
#' @param classification_metrics_used A character vector of the names of the classification
#'  metrics used to evaluate the linkage algorithm. Will be used in the Summary
#'  and Methods sections of the report.
#' @param ground_truth String indicating the ground truth used to produce the
#'  performance meassures.
#' @param ground_truth_missing_var A string of the name of the variable indicating
#'  the missingness of the ground truth variable. Must be present in either \code{main_data}
#'  or \code{missing_data_indicators}.
#' @param abbreviations A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. Data must contain two columns:
#'  the list of abbreviations in the first and their definitions in the second.
#' @param abbreviations_display_header A logical indicating whether to display the
#'  column headers in the abbreviation table. Only applied when \code{output_format =}
#'  "\code{docx}".
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#' @param display_percent_symbol A logical indicating whether to display a percent symbol
#' next to percentages in the linkage rate and missingness tables. Default is \code{FALSE}.
#' @param text_font_size A number specifying the font size for the inline text.
#'  Default is \code{12}.
#' @param table_font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param cover_page A file path to a png, pdf or jpg file that contains the
#'  desired cover page. Default is \code{system.file("background_images", "cover_page.pdf", package = "linkrep")}.
#' @param content_portrait_page A file path to a png, pdf or jpg file that contains the
#'  desired content portrait page. Default is \code{system.file("background_images", "content_portrait_page.pdf", package = "linkrep")}.
#' @param content_landscape_page A file path to a png, pdf or jpg file that contains the
#'  desired content landscape page. Default is \code{system.file("background_images", "content_landscape_page.pdf", package = "linkrep")}.
#' @param display_back_cover_page A logical indicating whether to display the back
#'  cover page in the output.
#' @param back_cover_page A file path to a png, pdf or jpg file that contains the
#'  desired back cover page. Default is \code{system.file("background_images", "back_cover_page.pdf", package = "linkrep")}.
#' @param temp_data_output_dir A path to a directory. All data frames must be passed
#'  into the quarto document (report generator) as an rds file therefore, if any data
#'  passed in is a data frame, those temporary rds files will be stored in this
#'  directory. Default is \code{tempdir(check = TRUE)}
#' @param quarto_report_template A file path to a quarto (qmd) file that renders
#'  the report. Default is \code{system.file("templates", "base_quarto_report_template.qmd", package = "linkrep")}.
#' @param references A file path to a BibTex (bib) file that contains the references
#'  used in the report. For references to be displayed in the References section of
#'  the report, they must be used in a citation elsewhere. Default is
#'  \code{system.file("templates", "references.bib", package = "linkrep")}.
#' @param word_template A file path a to a word document that specifies the output
#'  styles for a word report. Default is \code{system.file("templates", "word_template.docx", package = "linkrep")}.
#' @param set_background_images_template A file path to a LaTex (tex) document that
#'  specifies how the background images are laid onto a pdf report.
#'
#' @details
#' All tables display the variable labels in their headings before reverting to
#'  the raw variable name. To label variables in your data before calling this
#'  function use \code{\link[Hmisc]{label}}.
#'
#' Information on \code{missing_data_indicators}:\cr
#' Used in the linkage rate table and the missingness table. All variables present
#'  in the data will be displayed in both tables.\cr
#' Linkage rate table:\cr
#'  Variables associated with those in \code{main_data} must either have the same
#'  variable name suffixed by "\code{_missing}" or have the same label for it to be
#'  displayed in the linkage rate table as a value of that variable. In this case,
#'  the variable will be relabelled "\code{Missing}" and tabbed under the header of
#'  the variable it's associated with. If the variable is not associated with one
#'  in \code{main_data} it will be relabelled with its label or variable name
#'  prefixed by "\code{Missing }".\cr
#' Missingness table:\cr
#' No label changes are made in this table. It is made entirely up of the data
#'  from this dataset.
#'
#' Information on \code{save_linkage_rate}:\cr
#'  If \code{save_linkage_rate = TRUE}, an SQLite file (linkage_rate.sqlite) will be
#'  saved in the \code{output_dir}.\cr
#'  The file will include the following:\cr
#'  - The report generation date\cr
#'  - The report generation year\cr
#'  - The name of the data linker (provided by \code{data_linker})\cr
#'  - The name of the left dataset (provided by \code{left_dataset_name})\cr
#'  - The name fo the right dataset (provided by \code{right_dataset_name})\cr
#'  - The overall linkage rate\cr
#'  - The acquisition dates of the left dataset\cr
#'  - The project ID\cr
#'
#' @return Saves either a word or pdf record linkage quality report in the \code{output_dir}.\cr
#'  If \code{save_linkage_rate = TRUE}, an SQLite file (linkage_rate.sqlite) containing
#'  the linkage rate and other important information will be saved in the same location.\cr
#'  If \code{linkage_rate_tbl_output_to_csv = TRUE}, a csv file (linkage_rate_table.csv)
#'  containing the linkage rate table will be saved in the same location.
#' @export
#'
#' @importFrom data.table is.data.table
#' @importFrom xfun file_ext
#' @importFrom quarto quarto_render
#'
linkage_quality_report <- function(main_data,
                                   report_title,
                                   left_dataset_name,
                                   right_dataset_name,
                                   output_dir,
                                   data_linker,
                                   linkage_rate_tbl_column_var,
                                   linkage_rate_tbl_strata_vars,
                                   linkage_rate_tbl_footnotes = NULL,
                                   linkage_rate_tbl_display_total_column = TRUE,
                                   linkage_rate_tbl_display_mean_not_median_stats = FALSE,
                                   linkage_rate_tbl_display_alphabetically = FALSE,
                                   linkage_rate_tbl_output_to_csv = FALSE,
                                   missing_data_indicators = NULL,
                                   missingness_tbl_footnotes = NULL,
                                   output_format = "pdf",
                                   save_linkage_rate = TRUE,
                                   project_id = NULL,
                                   num_records_right_dataset = NULL,
                                   acquisition_year_var = NULL,
                                   acquisition_month_var = NULL,
                                   algorithm_summary_data = NULL,
                                   algorithm_summary_tbl_footnotes = NULL,
                                   performance_measures_data = NULL,
                                   performance_measures_tbl_footnotes = NULL,
                                   classification_metrics_used = NULL,
                                   ground_truth = NULL,
                                   ground_truth_missing_var = NULL,
                                   abbreviations = NULL,
                                   abbreviations_display_header = TRUE,
                                   thousands_separator = ",",
                                   decimal_mark = ".",
                                   num_decimal_places = 1,
                                   display_percent_symbol = FALSE,
                                   text_font_size = 12,
                                   table_font_size = 12,
                                   font_style = "Times New Roman",
                                   cover_page = NULL,
                                   content_portrait_page = NULL,
                                   content_landscape_page = NULL,
                                   display_back_cover_page = TRUE,
                                   back_cover_page = NULL,
                                   temp_data_output_dir = tempdir(check = TRUE),
                                   quarto_report_template = NULL,
                                   references = NULL,
                                   word_template = NULL,
                                   set_background_images_template = NULL
){


  # data input checks

  #----
  # generate_data_path
  #
  # data: data to validate
  # parameter: name of the parameter being checked
  # validates the data -> data must be a path to a csv or rds file or a dataframe.
  # if the data is a dataframe it gets saved in a temporary rds file.
  # return: [1] the path to the data, [2] a logical indicating whether a temporary file was generated
  #----
  generate_data_path <- function(data, parameter) {
    temp_path <- FALSE
    if (is.data.frame(data) | is.data.table(data)) {
      # save dataframe into an rds file temporarily
      path <- tempfile(tmpdir = temp_data_output_dir, fileext = ".rds")
      temp_path <- TRUE
      saveRDS(data, path)
    } else if (is.character(data)) {
      if (length(data) != 1){
        stop(sprintf("Invalid argument: %s. Must be single character string", parameter))
      }
      if (!file.exists(data)) {
        stop(sprintf("Invalid argument: %s. File not found", parameter))
      }
      if (file.info(data)$isdir) {
        stop(sprintf(
          "Invalid argument: %s. File extension must be either .rds or .csv",
          parameter
        ))
      }
      if (file_ext(data) != "rds" &
          file_ext(data) != "csv") {
        stop(sprintf(
          "Invalid argument: %s. File extension must be either .rds or .csv",
          parameter
        ))
      }
      path <- data
    } else {
      stop(
        sprintf(
          "Invalid argument: %s. %s must be a dataframe or a file path to a csv or rds file",
          parameter, parameter
        )
      )
    }
    return(c(path, temp_path))
  }

  if (!is.null(temp_data_output_dir)){
    validate_string(temp_data_output_dir, "temp_data_output_dir")
    if (!dir.exists(temp_data_output_dir)) {
      stop(
        "Invalid argument: temp_data_output_dir. temp_data_output_dir must be a path to a directory"
      )
    }
  }

  # main_data
  path <- generate_data_path(main_data, "main_data")
  main_data_path <- path[1]
  main_data_used_temp_path <- path[2]

  # missing_data_indicators
  if (is.null(missing_data_indicators) & !is.null(missingness_tbl_footnotes)){
    warning("Footnotes were provided for the missingness table with no 'missing_data_indicators'. Table will not be created.")
  }
  mis_data_ind_path <- NULL
  mis_data_ind_used_temp_path <- NULL
  if (!is.null(missing_data_indicators)){
    path <- generate_data_path(missing_data_indicators, "missing_data_indicators")
    mis_data_ind_path <- path[1]
    mis_data_ind_used_temp_path <- path[2]
  }

  # algorithm_summary_data
  if (is.null(algorithm_summary_data) & !is.null(algorithm_summary_tbl_footnotes)){
    warning("Footnotes were provided for the algorithm summary table with no 'algorithm_summary_data'. Table will not be created.")
  }
  alg_summ_path <- NULL
  alg_summ_used_temp_path <- NULL
  if (!is.null(algorithm_summary_data)){
    path <- generate_data_path(algorithm_summary_data, "algorithm_summary_data")
    alg_summ_path <- path[1]
    alg_summ_used_temp_path <- path[2]
  }

  # performance_measures_data
  if (is.null(performance_measures_data) & !is.null(performance_measures_tbl_footnotes)){
    warning("Footnotes were provided for the performance measures table with no 'performance_measures_data'. Table will not be created.")
  }
  perf_meas_path <- NULL
  perf_meas_used_temp_path <- NULL
  if (!is.null(performance_measures_data)){
    if (is.null(ground_truth) | is.null(ground_truth_missing_var)){
      stop("Must provide ground_truth and ground_truth_missing_var with 'performance_measures_data'")
    }
    path <- generate_data_path(performance_measures_data, "performance_measures_data")
    perf_meas_path <- path[1]
    perf_meas_used_temp_path <- path[2]
  }

  # abbreviations
  abbrev_tbl_path <- NULL
  abbrev_tbl_used_temp_path <- NULL
  if (!is.null(abbreviations)) {
    validate_boolean(abbreviations_display_header, "abbreviations_display_header")
    path <- generate_data_path(abbreviations, "abbreviations")
    abbrev_tbl_path <- path[1]
    abbrev_tbl_used_temp_path <- path[2]
  }


  # other parameter checks
  validate_string(report_title, "report_title")
  validate_string(left_dataset_name, "left_dataset_name")
  validate_string(right_dataset_name, "right_dataset_name")

  validate_string(output_dir, "output_dir")
  if (!dir.exists(output_dir)) {
    stop("Invalid argument: output_dir. output_dir must be a path to a directory")
  }

  validate_string(data_linker, "data_linker")

  validate_string(linkage_rate_tbl_column_var, "linkage_rate_tbl_column_var")
  validate_string_vector(linkage_rate_tbl_strata_vars, "linkage_rate_tbl_strata_vars")
  if (!is.null(linkage_rate_tbl_footnotes)){
    validate_string_vector(linkage_rate_tbl_footnotes, "linkage_rate_tbl_footnotes")
  }
  validate_boolean(linkage_rate_tbl_display_total_column, "linkage_rate_tbl_display_total_column")
  validate_boolean(linkage_rate_tbl_display_mean_not_median_stats, "linkage_rate_tbl_display_mean_not_median_stats")
  validate_boolean(linkage_rate_tbl_display_alphabetically, "linkage_rate_tbl_display_alphabetically")
  validate_boolean(linkage_rate_tbl_output_to_csv, "linkage_rate_tbl_output_to_csv")

  if (!is.null(missingness_tbl_footnotes)){
    validate_string_vector(missingness_tbl_footnotes, "missingness_tbl_footnotes")
  }

  validate_boolean(save_linkage_rate, "save_linkage_rate")

  if (!is.null(num_records_right_dataset)){
    validate_numeric(num_records_right_dataset, "num_records_right_dataset")
    if (num_records_right_dataset <= 0){
      stop("Invalid argument: num_records_right_dataset. num_records_right_dataset must be > 0")
    }
  }

  if (!is.null(acquisition_year_var)){
    validate_string(acquisition_year_var, "acquisition_year_var")
  }
  if (!is.null(acquisition_month_var)){
    validate_string(acquisition_month_var, "acquisition_month_var")
  }
  if (is.null(acquisition_year_var) & !is.null(acquisition_month_var)){
    stop("'acquisition_year_var' must be provided with 'acquisition_month_var'")
  }

  if (!is.null(algorithm_summary_tbl_footnotes)){
    validate_string_vector(algorithm_summary_tbl_footnotes, "algorithm_summary_tbl_footnotes")
  }

  if (!is.null(performance_measures_tbl_footnotes)){
    validate_string_vector(performance_measures_tbl_footnotes, "performance_measures_tbl_footnotes")
  }
  if (!is.null(classification_metrics_used)){
    validate_string_vector(classification_metrics_used, "classification_metrics_used")
  }

  if (!is.null(ground_truth)){
    validate_string(ground_truth, "ground_truth")
  }
  if (!is.null(ground_truth_missing_var)){
    validate_string(ground_truth_missing_var, "ground_truth_missing_var")
  }

  validate_common_parameters(output_format = output_format,
                      thousands_separator = thousands_separator,
                      decimal_mark = decimal_mark,
                      num_decimal_places = num_decimal_places,
                      display_percent_symbol = display_percent_symbol,
                      font_style = font_style,
                      font_size = text_font_size)

  validate_common_parameters(font_size = table_font_size)

  #----
  # check_page_files
  #
  # page: the file to the background image for that page
  # parameter: name of the parameter being checked
  # Checks to see if the files provided is valid for the background image file types
  #----
  check_page_files <- function(page, parameter){
    validate_string(page, parameter)
    if (!file.exists(page)){
      stop(sprintf("Invalid argument: %s. File not found", parameter))
    }
    if (file_ext(page) != "png" & file_ext(page) != "pdf" &
        file_ext(page) != "jpg" & file_ext(page) != "jpeg") {
      stop(sprintf(
        "Invalid argument: %s. File extension must be either .png, .pdf, .jpg or .jpeg",
        parameter
      ))
    }
  }

  if (is.null(cover_page)){
    if (!file.exists(system.file("background_images", "cover_page.pdf", package = "linkrep"))){
      stop("Default cover page file not found. Check installation or if removed, ensure one is passed to the function.")
    }
    cover_page <- system.file("background_images", "cover_page.pdf", package = "linkrep")
  }
  check_page_files(cover_page, "cover_page")

  if (is.null(content_portrait_page)){
    if (!file.exists(system.file("background_images", "content_portrait_page.pdf", package = "linkrep"))){
      stop("Default content portrait page file not found. Check installation or if removed, ensure one is passed to the function.")
    }
    content_portrait_page <- system.file("background_images", "content_portrait_page.pdf", package = "linkrep")
  }
  check_page_files(content_portrait_page, "content_portrait_page")

  if (is.null(content_landscape_page)){
    if (!file.exists(system.file("background_images", "content_landscape_page.pdf", package = "linkrep"))){
      stop("Default content landscape page file not found. Check installation or if removed, ensure one is passed to the function.")
    }
    content_landscape_page <- system.file("background_images", "content_landscape_page.pdf", package = "linkrep")
  }
  check_page_files(content_landscape_page, "content_landscape_page")

  validate_boolean(display_back_cover_page, "display_back_cover_page")
  if (display_back_cover_page){
    if (is.null(back_cover_page)){
      if (!file.exists(system.file("background_images", "back_cover_page.pdf", package = "linkrep"))){
        stop("Default back cover page file not found. Check installation or if removed, ensure one is passed to the function.")
      }
      back_cover_page <- system.file("background_images", "back_cover_page.pdf", package = "linkrep")
    }
    check_page_files(back_cover_page, "back_cover_page")
  }

  if (!file.exists(system.file("background_images", "acknowledgements_page.pdf", package = "linkrep"))){
    stop("Default acknowledgements page file not found. Check installation, ensure file is present in package by checking 'background_images' folder in location produced by `system.file(package = 'linkrep')`.")
  }
  acknowledgements_page <- system.file("background_images", "acknowledgements_page.pdf", package = "linkrep")
  check_page_files(acknowledgements_page, "acknowledgements_page")

  if (is.null(quarto_report_template)){
    if (!file.exists(system.file("templates", "base_quarto_report_template.qmd", package = "linkrep"))){
      stop("Default quarto report not found. Check installation or if removed, ensure one is passed to the function.")
    }
    quarto_report_template <- system.file("templates", "base_quarto_report_template.qmd", package = "linkrep")
  }
  validate_string(quarto_report_template, "quarto_report_template")
  if (!file.exists(quarto_report_template)){
    stop("Invalid argument: quarto_report_template. File not found")
  }
  if (file_ext(quarto_report_template) != "qmd"){
    stop("Invalid argument: quarto_report_template. File extension must be .qmd")
  }

  if (is.null(set_background_images_template)){
    if (!file.exists(system.file("templates", "set_background_images.tex", package = "linkrep"))){
      stop("Default set background images files not found. Check installation or if removed, ensure one is passed to the function.")
    }
    set_background_images_template <- system.file("templates", "set_background_images.tex", package = "linkrep")
  }
  validate_string(set_background_images_template, "set_background_images_template")
  if (!file.exists(set_background_images_template)){
    stop("Invalid argument: set_background_images_template. File not found")
  }
  if (file_ext(set_background_images_template) != "tex"){
    stop("Invalid argument: set_background_images_template. File extension must be .tex")
  }

  if (is.null(references)){
    if (!file.exists(system.file("templates", "references.bib", package = "linkrep"))){
      stop("Default references file not found. Check installation or if removed, ensure one is passed to the function.")
    }
    references <- system.file("templates", "references.bib", package = "linkrep")
  }
  validate_string(references, "references")
  if (!file.exists(references)){
    stop("Invalid argument: references. File not found")
  }
  if (file_ext(references) != "bib"){
    stop("Invalid argument: references. File extension must be .bib")
  }

  if (is.null(word_template)){
    if (!file.exists(system.file("templates", "word_template.docx", package = "linkrep"))){
      stop("Default word template not found. Check installation or if removed, ensure one is passed to the function.")
    }
    word_template <- system.file("templates", "word_template.docx", package = "linkrep")
  }
  validate_string(word_template, "word_template")
  if (!file.exists(word_template)){
    stop("Invalid argument: word_template. File not found")
  }
  if (file_ext(word_template) != "docx"){
    stop("Invalid argument: word_template. File extension must be .docx")
  }

  # substitute values into placeholders

  # ensure the file paths work in the LaTex commands as LaTex doesn't read '\\' as a file path separator
  cover_page <- gsub("\\\\", "/", cover_page)
  content_portrait_page <- gsub("\\\\", "/", content_portrait_page)
  content_landscape_page <- gsub("\\\\", "/", content_landscape_page)
  set_bg_images_lines <- readLines(set_background_images_template)

  if (display_back_cover_page){
    # set the command within the LaTex file
    if (!any(grepl("fancy_header_cmd", set_bg_images_lines))) {
      stop("In the file setting the background images, the placeholder for the fancy header command must be 'fancy_header_cmd'")
    }
    set_bg_images_lines <- gsub("fancy_header_cmd", "\\\\setbgimagewithback", set_bg_images_lines)

    back_cover_page <- gsub("\\\\", "/", back_cover_page)
    if (!any(grepl("back_page", set_bg_images_lines))) {
      stop("In the file setting the background images, the placeholder for the back cover page must be 'back_cover'")
    }
    set_bg_images_lines <- gsub("back_page", back_cover_page, set_bg_images_lines)

    # placeholders used within the LaTex file for the \setbgimagewithback command
    cover <- "cover_page_with_back"
    content_port <- "content_portrait_page_with_back"
    content_land <- "content_landscape_page_with_back"
  } else {
    # set the command within the LaTex file
    if (!any(grepl("fancy_header_cmd", set_bg_images_lines))) {
      stop("In the file setting the background images, the placeholder for the fancy header command must be 'fancy_header_cmd'")
    }
    set_bg_images_lines <- gsub("fancy_header_cmd", "\\\\setbgimagenoback", set_bg_images_lines)

    # placeholders used within the LaTex file for the \setbgimagewithback command
    cover <- "cover_page_no_back"
    content_port <- "content_portrait_page_no_back"
    content_land <- "content_landscape_page_no_back"
  }

  if (!any(grepl(cover, set_bg_images_lines))) {
    stop("In the file setting the background images, the placeholder for the cover page must be either 'cover_page_with_back' or 'cover_page_no_back'")
  }
  set_bg_images_lines <- gsub(cover, cover_page, set_bg_images_lines)

  if (!any(grepl(content_port, set_bg_images_lines))) {
    stop("In the file setting the background images, the placeholder for the content portrait page must be either 'content_portrait_page_with_back' or 'content_portrait_page_no_back'")
  }
  set_bg_images_lines <- gsub(content_port, content_portrait_page, set_bg_images_lines)

  if (!any(grepl(content_land, set_bg_images_lines))) {
    stop("In the file setting the background images, the placeholder for the content landscape page must be either 'content_landscape_page_with_back' or 'content_landscape_page_no_back'")
  }
  set_bg_images_lines <- gsub(content_land, content_landscape_page, set_bg_images_lines)

  if (!any(grepl("acknowledgements_page", set_bg_images_lines))) {
    stop("In the file setting the background images, the placeholder for the acknowledgements page must be 'acknowledgements'")
  }
  set_bg_images_lines <- gsub("acknowledgements_page", acknowledgements_page, set_bg_images_lines)

  dir <- system.file("templates", package = "linkrep")
  if (!dir.exists(dir)){
    stop("Trying to save updated setting background images file in non-existent directory. Ensure 'templates' folder exists in package.")
  }

  new_set_background_images_template <- file.path(dir, "updated_set_bg_images.tex")
  writeLines(set_bg_images_lines, new_set_background_images_template)

  # set values in quarto template
  quarto_report <- readLines(quarto_report_template)
  text_font_size <- paste0(text_font_size, "pt")

  if (!any(grepl("\\{fontsize\\}", quarto_report))) {
    stop("In the quarto template report file, the placeholder for the text font size must be '{fontsize}' in the YAML header")
  }
  quarto_report <- gsub("\\{fontsize\\}", text_font_size, quarto_report)

  if (!any(grepl("\\{mainfont\\}", quarto_report)) | !any(grepl("\\{sansfont\\}", quarto_report))) {
    stop("In the quarto template report file, the placeholders for the font type must be '{mainfont}' and '{sansfont}' in the YAML header")
  }
  quarto_report <- gsub("\\{mainfont\\}", font_style, quarto_report)
  quarto_report <- gsub("\\{sansfont\\}", font_style, quarto_report)

  if (!any(grepl("\\{background_images\\}", quarto_report))) {
    stop("In the quarto template report file, the placeholder for the LaTex file that sets the background images must be '{background_images}' in the YAML header")
  }
  quarto_report <- gsub("\\{background_images\\}", new_set_background_images_template, quarto_report)

  if (!any(grepl("\\{references\\}", quarto_report))) {
    stop("In the quarto template report file, the placeholder for the word template must be '{references}' in the YAML header")
  }
  quarto_report <- gsub("\\{references\\}", references, quarto_report)

  if (!any(grepl("\\{word_template\\}", quarto_report))) {
    stop("In the quarto template report file, the placeholder for the word template must be '{word_template}' in the YAML header")
  }
  quarto_report <- gsub("\\{word_template\\}", word_template, quarto_report)

  dir <- system.file("templates", package = "linkrep")
  if (!dir.exists(dir)){
    stop("Trying to save updated quarto file in non-existent directory. Ensure 'templates' folder exists in package.")
  }
  updated_quarto_report <- file.path(dir, "updated_quarto_report.qmd")
  writeLines(quarto_report, updated_quarto_report)


  quarto_render(
    input = updated_quarto_report,
    output_format = output_format,
    execute_params = list(
      main_data_path = main_data_path,
      main_data_used_temp_path = main_data_used_temp_path,
      report_title = report_title,
      left_dataset_name = left_dataset_name,
      right_dataset_name = right_dataset_name,
      output_dir = output_dir,
      data_linker = data_linker,
      linkage_rate_tbl_col_var = linkage_rate_tbl_column_var,
      linkage_rate_tbl_strata_vars = linkage_rate_tbl_strata_vars,
      linkage_rate_tbl_footnotes = linkage_rate_tbl_footnotes,
      linkage_rate_tbl_display_total_column = linkage_rate_tbl_display_total_column,
      linkage_rate_tbl_display_mean_not_median_stats = linkage_rate_tbl_display_mean_not_median_stats,
      linkage_rate_tbl_display_alphabetically = linkage_rate_tbl_display_alphabetically,
      linkage_rate_tbl_output_to_csv = linkage_rate_tbl_output_to_csv,
      missing_data_indicators_path = mis_data_ind_path,
      missing_data_indicators_used_temp_path = mis_data_ind_used_temp_path,
      missingness_tbl_footnotes = missingness_tbl_footnotes,
      output_format = output_format,
      save_linkage_rate = save_linkage_rate,
      project_id = project_id,
      num_records_right_dataset = num_records_right_dataset,
      acquisition_year_var = acquisition_year_var,
      acquisition_month_var = acquisition_month_var,
      algorithm_summary_data_path = alg_summ_path,
      algorithm_summary_data_used_temp_path = alg_summ_used_temp_path,
      algorithm_summary_tbl_footnotes = algorithm_summary_tbl_footnotes,
      performance_measures_data_path = perf_meas_path,
      performance_measures_data_used_temp_path = perf_meas_used_temp_path,
      performance_measures_tbl_footnotes = performance_measures_tbl_footnotes,
      classification_metrics_used = classification_metrics_used,
      ground_truth = ground_truth,
      ground_truth_missing_var = ground_truth_missing_var,
      abbreviations_data_path = abbrev_tbl_path,
      abbreviations_data_used_temp_path = abbrev_tbl_used_temp_path,
      abbreviations_display_header = abbreviations_display_header,
      thousands_separator = thousands_separator,
      decimal_mark = decimal_mark,
      num_decimal_places = num_decimal_places,
      display_percent_symbol = display_percent_symbol,
      table_font_size = table_font_size,
      font_style = font_style,
      display_back_cover_page = display_back_cover_page
    )
  )

  file_name <- paste0("Record Linkage Quality Report.", output_format)
  src <- gsub("qmd", output_format, updated_quarto_report)
  result <- file.rename(src, paste0(output_dir, "/", file_name))
  message(paste0("Ignore above, output created: ", file_name))
  unlink(updated_quarto_report)

}




