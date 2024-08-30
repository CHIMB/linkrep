#' Generate a Record Linkage Quality Report
#'
#' The \code{linkage_quality_report} function takes in data from a linkage and
#' outputs a record linkage quality report. The report contains information on
#' record linkage and provides plots and tables to describe the data.
#'
#' @param main_data A data frame, a file path to an rds file that contains a data
#'  frame or a file path to a csv file. This data contains variables present
#'  in the left dataset of the linkage.
#' @param report_title String indicating the title of the report. If
#'  \code{output_format = "docx"}, the title will only be used in the suggested citation.
#' @param report_subtitle String indicating the subtitle of the report. If
#'  \code{output_format = "docx"}, the subtitle will only be used in the suggested citation.
#' @param left_dataset_name String indicating the name of the left dataset.
#' @param right_dataset_name String indicating the name of the right dataset.
#' @param output_dir A path to a directory. All output files will be save here.
#' @param data_linker String indicating who performed the linkage.
#' @param linkage_package String indicating the R package used to link the data.
#' @param stratified_linkage_tbls_column_var A string of the name of a logical or binary
#'  variable present in \code{main_data} that indicates whether a record linked or not.
#'  Its values will be the columns in the linkage rate table and the linkaed data
#'  representativeness table.
#' @param linked_data_representativeness_tbl_strata_vars A character vector of the
#'  names of the variables present in \code{main_data} to stratify the linked data
#'  representativeness table by.
#' @param linkage_rate_tbl_strata_vars A character vector of the names of the
#'  variables present in \code{main_data} to stratify the linkage rate table by.
#' @param linked_data_representativeness_tbl_footnotes A character vector of additional
#'  footnotes for the linked data representativeness table. Each element in the
#'  vector will be displayed on a new line.
#' @param linkage_rate_tbl_footnotes A character vector of additional footnotes for
#'  the linkage rate table. Each element in the vector will be displayed on a new line.
#' @param stratified_linkage_tbls_continuous_stat A string indicating which statistic
#'  to use on continuous variables in the linkage rate table. Allowed values are
#'  "\code{mean}" or "\code{median}" (default). If "\code{mean}", mean \eqn{\pm}
#'  standard deviation will be output otherwise, median (Q1, Q3), where Q1 is the
#'  25\eqn{^{th}} percentile, and Q3 is the 75\eqn{^{th}} percentile, will be output.
#' @param stratified_linkage_tbls_output_to_csv A logical indicating whether to
#'  save the linkage rate table in a csv file. Default is \code{FALSE}.
#' @param display_missingness_table A logical indicating whether to display the
#'  missingness table in the report.
#' @param missing_data_indicators A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. All variables in the data
#'  must be logical or binary, with \code{1} or \code{TRUE} representing a missing record
#'  for that variable. See Details section for more information on naming
#'  conventions and where this data is used in the report.
#' @param missingness_tbl_footnotes A character vector of additional footnotes for
#' the missingness table. Each element in the vector will be displayed on a new line.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param linkage_package_version String indicating the version of the \code{linkage_package}
#'  used to link the data. You can obtain the package version via \code{packageVersion("package_name")}.
#' @param linkrep_package_version String indicating the version of \code{linkrep} used to
#'  generate the report. You can obtain the package version via \code{packageVersion("linkrep")}.
#' @param R_version String indicating the version of R used to generate the report.
#'  You can obtain the version of R via \code{R.version.string}.
#' @param datastan_package_version String indicating the version of \code{datastan} used to
#'  preprocess the data. You can obtain the package version via \code{packageVersion("datastan")}.
#' @param comprehensive_report A logical indicating whether to output a comprehensive
#'  report. A comprehensive report includes the Background and Methods sections.
#' @param save_linkage_rate A logical indicating whether to save information on
#'  the linkage in an SQLite file. See Details section for details on what is
#'  saved in the file.
#' @param project_id String indicating the project ID.
#' @param num_records_right_dataset The number of records in the right dataset of the linkage.
#' @param acquisition_year_var A string of the name of the numeric variable in
#'  \code{main_data} that represents the acquisition year. This must be provided
#'  for the datasets date range to be output on the title page of a PDF output.
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
#' @param ground_truth String indicating the ground truth used to produce the
#'  performance measures. Must be provided with \code{performance_measures_data}.
#' @param num_pairs_non_missing_ground_truth The number of record pairs with non-missing
#'  ground truth.
#' @param num_record_pairs The total number of record pairs (cartesian product).
#' @param definitions A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. Data must contain two columns:
#'  the list of terms in the first and their definitions in the second.
#' @param definitions_display_header A logical indicating whether to display the
#'  column headers in the definitions table. Only applied when \code{output_format = "docx"}.
#' @param abbreviations A data frame, a file path to an rds file that
#'  contains a data frame or a file path to a csv file. Data must contain two columns:
#'  the list of abbreviations in the first and their meaning in the second.
#' @param abbreviations_display_header A logical indicating whether to display the
#'  column headers in the abbreviation table. Only applied when \code{output_format = "docx"}.
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
#'  desired cover page. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/background_images/cover_page.pdf}.
#' @param content_portrait_page A file path to a png, pdf or jpg file that contains the
#'  desired content portrait page. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/background_images/content_portrait_page.pdf}.
#' @param content_landscape_page A file path to a png, pdf or jpg file that contains the
#'  desired content landscape page. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/background_images/content_landscape_page.pdf}.
#' @param display_back_cover_page A logical indicating whether to display the back
#'  cover page in the output.
#' @param back_cover_page A file path to a png, pdf or jpg file that contains the
#'  desired back cover page. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/background_images/back_cover_page.pdf}.
#' @param blank_background A logical indicating whether to display the report on blank
#'  white background. Default is \code{FALSE}.
#' @param temp_data_output_dir A path to a directory. All complex data (ex. tables)
#'  must be passed into the quarto report through an rds file therefore, all
#'  temporarily generated rds files containing the report elements will be stored
#'  in this directory. Default is \code{tempdir(check = TRUE)}
#' @param quarto_report_template A file path to a quarto (qmd) file that renders
#'  the report. Use this parameter to apply additional customization to the report
#'  output. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/templates/base_quarto_report_template.qmd}.
#' @param extra_textual_content_quarto_template A file path to a quarto (qmd) file
#'  that contains the Background and Methods sections of the report.Default is
#'  \code{system.file("templates", "base_quarto_report_template.qmd", package = "linkrep")}.
#' @param references A file path to a BibTex (bib) file that contains the references
#'  used in the report. For references to be displayed in the References section of
#'  the report, they must be cited in the quarto document. Default is
#'  \url{https://github.com/CHIMB/linkrep/blob/main/inst/templates/references.bib}.
#' @param word_template A file path a to a word document that specifies the output
#'  styles for a word report. Default is \url{https://github.com/CHIMB/linkrep/blob/main/inst/templates/word_template.docx}.
#' @param set_background_images_template A file path to a LaTeX file that specifies
#'  how the background images are placed onto a PDF report. Default is
#'  \url{https://github.com/CHIMB/linkrep/blob/main/inst/templates/set_background_images.tex}.
#' @param citation_style A file path to a csl file containing the citation style.
#'  To find different styles visit \url{https://github.com/citation-style-language/styles/blob/master/american-medical-association.csl}
#'  If the location of the citation must change in the text you must modify its
#'  location in the quarto report template.
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
#' @return Saves either a word or pdf record linkage quality report to the \code{output_dir}.\cr
#'
#'  If \code{save_linkage_rate = TRUE}, an SQLite file (linkage_rate.sqlite) containing
#'  the linkage rate and other important information will be saved in \code{output_dir}.\cr
#'
#'  If \code{linkage_rate_tbl_output_to_csv = TRUE}, a csv file (linkage_rate_table.csv)
#'  containing the linkage rate table will be saved in \code{output_dir}.
#'
#' @export
#'
#' @importFrom data.table is.data.table fread
#' @importFrom xfun file_ext
#' @importFrom quarto quarto_render
#' @importFrom grDevices dev.off png pdf
#'
linkage_quality_report <- function(main_data,
                                   report_title,
                                   report_subtitle,
                                   left_dataset_name,
                                   right_dataset_name,
                                   output_dir,
                                   data_linker,
                                   linkage_package,
                                   stratified_linkage_tbls_column_var,
                                   linked_data_representativeness_tbl_strata_vars,
                                   linkage_rate_tbl_strata_vars,
                                   linked_data_representativeness_tbl_footnotes = NULL,
                                   linkage_rate_tbl_footnotes = NULL,
                                   stratified_linkage_tbls_continuous_stat = "median",
                                   stratified_linkage_tbls_output_to_csv = FALSE,
                                   display_missingness_table = FALSE,
                                   missing_data_indicators = NULL,
                                   missingness_tbl_footnotes = NULL,
                                   output_format = "pdf",
                                   linkage_package_version = NULL,
                                   linkrep_package_version = NULL,
                                   R_version = NULL,
                                   datastan_package_version = NULL,
                                   comprehensive_report = TRUE,
                                   save_linkage_rate = TRUE,
                                   project_id = NULL,
                                   num_records_right_dataset = NULL,
                                   acquisition_year_var = NULL,
                                   acquisition_month_var = NULL,
                                   algorithm_summary_data = NULL,
                                   algorithm_summary_tbl_footnotes = NULL,
                                   performance_measures_data = NULL,
                                   performance_measures_tbl_footnotes = NULL,
                                   ground_truth = NULL,
                                   num_pairs_non_missing_ground_truth = NULL,
                                   num_record_pairs = NULL,
                                   definitions = NULL,
                                   definitions_display_header = TRUE,
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
                                   blank_background = FALSE,
                                   temp_data_output_dir = tempdir(check = TRUE),
                                   quarto_report_template = NULL,
                                   extra_textual_content_quarto_template = NULL,
                                   references = NULL,
                                   word_template = NULL,
                                   set_background_images_template = NULL,
                                   citation_style = NULL
){

  # perform parameter input checks

  if (!is.null(temp_data_output_dir)){
    validate_string(temp_data_output_dir, "temp_data_output_dir")
    if (!dir.exists(temp_data_output_dir)) {
      stop(
        "Invalid argument: temp_data_output_dir. temp_data_output_dir must be a path to a directory"
      )
    }
  }
  # to word in quarto file path must be formatted as aa/aa/a and not aa\\aa\\a
  temp_data_output_dir <- gsub("\\\\", "/", temp_data_output_dir)

  validate_string(report_title, "report_title")
  validate_string(report_subtitle, "report_subtitle")
  validate_string(left_dataset_name, "left_dataset_name")
  validate_string(right_dataset_name, "right_dataset_name")

  validate_string(output_dir, "output_dir")
  if (!dir.exists(output_dir)) {
    stop("Invalid argument: output_dir. output_dir must be a path to a directory")
  }

  validate_string(data_linker, "data_linker")
  validate_string(linkage_package, "linkage_package")

  validate_string(stratified_linkage_tbls_column_var, "stratified_linkage_tbls_column_var")
  validate_string_vector(linked_data_representativeness_tbl_strata_vars, "linked_data_representativeness_tbl_strata_vars")
  if (!is.null(linked_data_representativeness_tbl_footnotes)){
    validate_string_vector(linked_data_representativeness_tbl_footnotes, "linked_data_representativeness_tbl_footnotes")
  }
  validate_string_vector(linkage_rate_tbl_strata_vars, "linkage_rate_tbl_strata_vars")
  if (!is.null(linkage_rate_tbl_footnotes)){
    validate_string_vector(linkage_rate_tbl_footnotes, "linkage_rate_tbl_footnotes")
  }
  validate_string(stratified_linkage_tbls_continuous_stat, "stratified_linkage_tbls_continuous_stat")
  if (stratified_linkage_tbls_continuous_stat != "median" & stratified_linkage_tbls_continuous_stat != "mean"){
    stop("Invalid argument: stratified_linkage_tbls_continuous_stat. Options: 'median' or 'mean'")
  }
  validate_boolean(stratified_linkage_tbls_output_to_csv, "stratified_linkage_tbls_output_to_csv")

  validate_boolean(display_missingness_table, "display_missingness_table")
  if (is.null(missing_data_indicators) & display_missingness_table){
    stop("display_missingness_table cannot be TRUE when no data is passed in for missing_data_indicators. Either pass data to the missing_data_indicators parameter or set display_missingness_table to FALSE")
  }
  if (!is.null(missingness_tbl_footnotes)){
    validate_string_vector(missingness_tbl_footnotes, "missingness_tbl_footnotes")
  }

  if (!is.null(linkage_package_version)){
    validate_string(linkage_package_version, "linkage_package_version")
  }
  if (!is.null(linkrep_package_version)){
    validate_string(linkrep_package_version, "linkrep_package_version")
  }
  if (!is.null(R_version)){
    validate_string(R_version, "R_version")
  }
  if (!is.null(datastan_package_version)){
    validate_string(datastan_package_version, "datastan_package_version")
  }

  validate_boolean(save_linkage_rate, "save_linkage_rate")

  if (!is.null(project_id)){
    if (length(project_id) != 1){
      stop("Invalid argument: project_id. project_id must be a single input.")
    }
  }

  validate_boolean(comprehensive_report, "comprehensive_report")

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

  if (!is.null(ground_truth)){
    validate_string(ground_truth, "ground_truth")
  }
  if (!is.null(num_pairs_non_missing_ground_truth)){
    if (is.null(num_record_pairs)){
      stop("num_pairs_non_missing_ground_truth and num_record_pairs must both be provided")
    } else {
      validate_numeric(num_pairs_non_missing_ground_truth, " num_pairs_non_missing_ground_truth")
      validate_numeric(num_record_pairs, "num_record_pairs")
      if (num_pairs_non_missing_ground_truth > num_record_pairs){
        stop("num_pairs_non_missing_ground_truth should not be larger than num_record_pairs")
      }
    }
  }

  validate_boolean(abbreviations_display_header, "abbreviations_display_header")
  validate_boolean(definitions_display_header, "definitions_display_header")
  validate_boolean(blank_background, "blank_background")

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

  # background is only affected in pdf output
  if (output_format == "pdf"){
    if (blank_background){
      if (!file.exists(system.file("background_images", "white_background.pdf", package = "linkrep"))){
        stop("Default blank page file not found. Check installation.")
      }
      blank_page <- system.file("background_images", "white_background.pdf", package = "linkrep")
      cover_page <- blank_page
      content_portrait_page <- blank_page
      content_landscape_page <- blank_page
      back_cover_page <- blank_page
    } else {
      if (is.null(cover_page)){
        if (!file.exists(system.file("background_images", "cover_page.pdf", package = "linkrep"))){
          stop("Default cover page file not found. Check installation or if removed, ensure one is passed to the function.")
        }
        cover_page <- system.file("background_images", "cover_page.pdf", package = "linkrep")
      }

      if (is.null(content_portrait_page)){
        if (!file.exists(system.file("background_images", "content_portrait_page.pdf", package = "linkrep"))){
          stop("Default content portrait page file not found. Check installation or if removed, ensure one is passed to the function.")
        }
        content_portrait_page <- system.file("background_images", "content_portrait_page.pdf", package = "linkrep")
      }

      if (is.null(content_landscape_page)){
        if (!file.exists(system.file("background_images", "content_landscape_page.pdf", package = "linkrep"))){
          stop("Default content landscape page file not found. Check installation or if removed, ensure one is passed to the function.")
        }
        content_landscape_page <- system.file("background_images", "content_landscape_page.pdf", package = "linkrep")
      }

      validate_boolean(display_back_cover_page, "display_back_cover_page")
      if (display_back_cover_page){
        if (is.null(back_cover_page)){
          if (!file.exists(system.file("background_images", "back_cover_page.pdf", package = "linkrep"))){
            stop("Default back cover page file not found. Check installation or if removed, ensure one is passed to the function.")
          }
          back_cover_page <- system.file("background_images", "back_cover_page.pdf", package = "linkrep")
        }
      }
    }

    check_page_files(cover_page, "cover_page")
    check_page_files(content_portrait_page, "content_portrait_page")
    check_page_files(content_landscape_page, "content_landscape_page")
    if (display_back_cover_page){
      check_page_files(back_cover_page, "back_cover_page")
    }

    if (!file.exists(system.file("background_images", "acknowledgements_page.pdf", package = "linkrep"))){
      stop("Default acknowledgements page file not found. Check installation, ensure file is present in package by checking 'background_images' folder in location produced by `system.file(package = 'linkrep')`.")
    }
    acknowledgements_page <- system.file("background_images", "acknowledgements_page.pdf", package = "linkrep")
    check_page_files(acknowledgements_page, "acknowledgements_page")
  }

  #----
  # check_template_files
  #
  # @param file The template file
  # @param extension The file extension
  # @param default_name The name of the default file for that template
  # @param param The name of the parameter being checked
  #
  # @return Either an error message or the file path
  #----
  check_template_files <- function(file, extension, default_name, param){
    if (is.null(file)){
      if (!file.exists(system.file("templates", default_name, package = "linkrep"))){
        stop(sprintf("Default %s filr not found. Check installation or if removed, ensure one is passed to the functoin.", param))
      }
      file <- system.file("templates", default_name, package = "linkrep")
    }
    validate_string(file, param)
    if (!file.exists(file)){
      stop(sprintf("Invalid argument: %s. File not found.", param))
    }
    if (file_ext(file) != extension){
      stop(sprintf("Invalid argument: %s. File extension must be .%s", param, extension))
    }
    return(file)
  }

  quarto_report_template <- check_template_files(quarto_report_template, "qmd", "base_quarto_report_template.qmd", "quarto_report_template")
  extra_textual_content_quarto_template <- check_template_files(extra_textual_content_quarto_template, "qmd", "extra_textual_content.qmd", "extra_textual_content_quarto_template")
  references <- check_template_files(references, "bib", "references.bib", "references")
  word_template <- check_template_files(word_template, "docx", "word_template.docx", "word_template")
  set_background_images_template <- check_template_files(set_background_images_template, "tex", "set_background_images.tex", "set_background_images_template")
  citation_style <- check_template_files(citation_style, "csl", "american-medical-association.csl", "citation_style")


  # read in data and perform checks

  #----
  # read_data
  #
  # Performs error checking on the file path and reads in the data.
  #
  # @param dataset_path A file path to the data.
  # @param parameter The name of the parameter whose path we're checking.
  #
  # @return The read in data.
  #----
  read_data <- function(data, parameter){
    validate_string(parameter, "parameter")
    if(is.data.frame(data) | is.data.table(data)){
      read_in_data <- data
    } else if (is.character(data)){
      if (length(data) != 1){
        stop(sprintf("Invalid argument: %s. %s must be a data frame or a file path to a csv or rds file."))
      }
      if (!file.exists(data)){
        stop(sprintf("Invalid argument: %s. File not found", parameter))
      }

      if (file_ext(data) == "rds"){
        read_in_data <- readRDS(data)
      } else if (file_ext(data) == "csv"){
        read_in_data <- fread(data)
      } else {
        stop(sprintf("Invalid argument: %s. File extension must be either .rds or .csv",
                     parameter))
      }
    } else {
      stop(sprintf("Invalid argument: %s. %s must be a data frame or a file path to a csv or rds file",
                   parameter, parameter))
    }
    return(read_in_data)
  }

  main_data <- read_data(main_data, "main_data")

  validate_var_in_data(stratified_linkage_tbls_column_var, main_data,
                       "stratified_linkage_tbls_column_var", "main_data")
  if (sum(is.na(main_data[[stratified_linkage_tbls_column_var]])) > 0 |
      sum(main_data[[stratified_linkage_tbls_column_var]] != 0 &
          main_data[[stratified_linkage_tbls_column_var]] != 1) > 0){
    stop("Invalid argument: stratified_linkage_tbls_column_var must be a binary or logical variable in 'main_data'")
  }

  invalid_strata_vars <- base::setdiff(linked_data_representativeness_tbl_strata_vars, names(main_data))
  if (length(invalid_strata_vars) > 0) {
    stop("Invalid argument: linked_data_representativeness_tbl_strata_vars. Not all variables provided are present in 'main_data'")
  }
  if (length(linked_data_representativeness_tbl_strata_vars) == 1){
    if (linked_data_representativeness_tbl_strata_vars == stratified_linkage_tbls_column_var){
      stop("stratified_linkage_tbls_column_var and linked_data_representativeness_tbl_strata_vars cannot be the same")
    }
  }

  invalid_strata_vars <- base::setdiff(linkage_rate_tbl_strata_vars, names(main_data))
  if (length(invalid_strata_vars) > 0) {
    stop("Invalid argument: linkage_rate_tbl_strata_vars. Not all variables provided are present in 'main_data'")
  }
  if (length(linkage_rate_tbl_strata_vars) == 1 & is.null(missing_data_indicators)){
    if (linkage_rate_tbl_strata_vars == stratified_linkage_tbls_column_var){
      stop("stratified_linkage_tbls_column_var and linkage_rate_tbl_strata_vars cannot be the same")
    }
  }

  acquisition_year <- NULL
  if (!is.null(acquisition_year_var)){
    validate_var_in_data(acquisition_year_var, main_data, "acquisition_year_var", "main_data")
    acquisition_year <- main_data[[acquisition_year_var]]
    if(!is.numeric(acquisition_year) & !is.integer(acquisition_year)){
      stop("Invalid argument: acquisition_year_var. acquisition_year_var must be a numeric variable")
    }
  }

  acquisition_month <- NULL
  if (!is.null(acquisition_month_var)){
    validate_var_in_data(acquisition_month_var, main_data, "acquisition_month_var", "main_data")
    acquisition_month <- main_data[[acquisition_month_var]]
    if(!is.numeric(acquisition_month) & !is.integer(acquisition_month)){
      stop("Invalid argument: acquisition_month_var. acquisition_month_var must be a numeric variable")
    }
    if (!all(acquisition_month %in% c(1:12, NA))){
      stop("acquisition_month_var contains invalid values. Must be either NA or numbers from 1 to 12.")
    }
  }

  if (is.null(missing_data_indicators) & !is.null(missingness_tbl_footnotes)){
    warning("Footnotes were provided for the missingness table with no 'missing_data_indicators'. Table will not be created.")
  }
  if (!is.null(missing_data_indicators)){
    missing_data_indicators <- read_data(missing_data_indicators, "missing_data_indicators")
    if (nrow(main_data) != nrow(missing_data_indicators)){
      stop("'main_data' and 'missing_data_indicators' should contain the same number of records")
    }
    validate_df_binary(missing_data_indicators, "missing_data_indicators")
  }

  if (!is.null(definitions)){
    definitions_data <- read_data(definitions, "definitions")
    if (ncol(definitions_data) != 2){
      stop("Invalid argument: abbreviations_data. abbreviations_data must have two columns, one for the abbreviations and one for their definitions")
    }
  }

  if (!is.null(abbreviations)){
    abbreviations_data <- read_data(abbreviations, "abbreviations")
    if (ncol(abbreviations_data) != 2){
      stop("Invalid argument: abbreviations_data. abbreviations_data must have two columns, one for the abbreviations and one for their definitions")
    }
  }

  if (is.null(algorithm_summary_data) & !is.null(algorithm_summary_tbl_footnotes)){
    warning("Footnotes were provided for the algorithm summary table with no 'algorithm_summary_data'. Table will not be created.")
  }
  if (!is.null(algorithm_summary_data)){
    algorithm_summary_data <- read_data(algorithm_summary_data, "algorithm_summary_data")
    }

  if (is.null(performance_measures_data) & !is.null(performance_measures_tbl_footnotes)){
    warning("Footnotes were provided for the performance measures table with no 'performance_measures_data'. Table will not be created.")
  }
  # ground_truth_missing <- NULL
  if (!is.null(performance_measures_data)){
    performance_measures_data <- read_data(performance_measures_data, "performance_measures_data")
    if (is.null(ground_truth)){
      stop("Must provide ground_truth with 'performance_measures_data'")
    }
  }

  # obtain linkage data dates
  data_time_period <- NULL

  if (!is.null(acquisition_year)){
    min_year <- min(acquisition_year, na.rm = TRUE)
    max_year <- max(acquisition_year, na.rm = TRUE)

    if (!is.null(acquisition_month)){
      #----
      # label_month
      #
      # assigns the month its English abbreviation
      #----
      label_month <- function(month){
        if (month == 1){
          month <- "Jan."
        } else if (month == 2){
          month <- "Feb."
        } else if (month == 3){
          month <- "Mar."
        } else if (month == 4){
          month <- "Apr."
        } else if (month == 5){
          month <- "May."
        } else if (month == 6){
          month <- "June."
        } else if (month == 7){
          month <- "July."
        } else if (month == 8){
          month <- "Aug."
        } else if (month == 9){
          month <- "Sept."
        } else if (month == 10){
          month <- "Oct."
        } else if (month == 11){
          month <- "Nov."
        } else {
          month <- "Dec."
        }
        return(month)
      }

      min_month <- min(acquisition_month[acquisition_year == min_year], na.rm = TRUE)
      min_month <- label_month(min_month)
      max_month <- max(acquisition_month[acquisition_year == max_year], na.rm = TRUE)
      max_month <- label_month(max_month)
    } else {
      min_month <- NULL
      max_month <- NULL
    }

    if (min_year == max_year){
      data_time_period <- min_year
    } else {
      if (is.null(min_month)){
        data_time_period <- paste(min_year, "-", max_year)
      } else {
        data_time_period <- paste(min_month, min_year, "-", max_month, max_year)
      }
    }
  }

  # calculate values needed throughout the report and format them to match arguments
  num_records_left_dataset <- nrow(main_data)
  num_records_left_dataset <- formatC(num_records_left_dataset,
                                      big.mark = thousands_separator,
                                      format = "f", digits = 0)

  if (!is.null(num_records_right_dataset)){
    num_records_right_dataset <- formatC(num_records_right_dataset,
                                         big.mark = thousands_separator,
                                         format = "f", digits = 0)
  }

  num_records_linked <- sum(main_data[[stratified_linkage_tbls_column_var]] == 1)
  num_records_linked <- formatC(num_records_linked,
                                big.mark = thousands_separator,
                                format = "f", digits = 0)

  overall_linkage_rate <- sum(main_data[[stratified_linkage_tbls_column_var]] == 1)/nrow(main_data) * 100
  overall_linkage_rate <- formatC(overall_linkage_rate, digits = num_decimal_places,
                                  big.mark = thousands_separator,
                                  decimal.mark = decimal_mark, format = "f")

  percent_non_missing_ground_truth <- NULL
  if (!is.null(num_pairs_non_missing_ground_truth) & !is.null(num_record_pairs)){
    percent_non_missing_ground_truth <- num_pairs_non_missing_ground_truth / num_record_pairs

    num_pairs_non_missing_ground_truth <- formatC(num_pairs_non_missing_ground_truth,
                                                  big.mark = thousands_separator,
                                                  format = "f", digits = 0)
    percent_non_missing_ground_truth <- formatC(percent_non_missing_ground_truth,
                                                big.mark = thousands_separator,
                                                decimal.mark = decimal_mark,
                                                digits = num_decimal_places,
                                                format = "f")
  }

  report_generation_date <- Sys.Date()
  report_generation_date <- format(report_generation_date, "%b. %d, %Y")

  #----
  # substitute values into placeholders
  #
  # The LaTeX file that sets up the background images needs to know the image paths
  # but, LaTeX files don't take parameters therefore, we put "placeholders" in the
  # file to indicate where a value needs to substituted. To substitute the image paths
  # into the function we read the lines of the LaTeX file and use gsub to sub the
  # file paths into their corresponding placeholders. Then we write the updated
  # lines into a new file for use in the quarto report.
  #----

  # background is only affected in pdf output
  new_set_background_images_template <- NULL
  if (output_format == "pdf"){
    # ensure the file paths work in the LaTex commands as LaTex doesn't read '\\' as a file path separator
    cover_page <- gsub("\\\\", "/", cover_page)
    content_portrait_page <- gsub("\\\\", "/", content_portrait_page)
    content_landscape_page <- gsub("\\\\", "/", content_landscape_page)
    set_bg_images_lines <- readLines(set_background_images_template)

    if (display_back_cover_page){
      # set the command within the LaTex file
      set_bg_images_lines <- gsub("fancy_header_cmd", "\\\\setbgimagewithback", set_bg_images_lines)

      back_cover_page <- gsub("\\\\", "/", back_cover_page)
      set_bg_images_lines <- gsub("back_page", back_cover_page, set_bg_images_lines)

      # placeholders used within the LaTex file for the \setbgimagewithback command
      cover <- "cover_page_with_back"
      content_port <- "content_portrait_page_with_back"
      content_land <- "content_landscape_page_with_back"
    } else {
      # set the command within the LaTex file
      set_bg_images_lines <- gsub("fancy_header_cmd", "\\\\setbgimagenoback", set_bg_images_lines)

      # placeholders used within the LaTex file for the \setbgimagewithback command
      cover <- "cover_page_no_back"
      content_port <- "content_portrait_page_no_back"
      content_land <- "content_landscape_page_no_back"
    }

    set_bg_images_lines <- gsub(cover, cover_page, set_bg_images_lines)
    set_bg_images_lines <- gsub(content_port, content_portrait_page, set_bg_images_lines)
    set_bg_images_lines <- gsub(content_land, content_landscape_page, set_bg_images_lines)

    if (!any(grepl("acknowledgements_page", set_bg_images_lines))) {
      stop("In the file setting the background images, the placeholder for the acknowledgements page must be 'acknowledgements'")
    }
    set_bg_images_lines <- gsub("acknowledgements_page", acknowledgements_page, set_bg_images_lines)

    # new_set_background_images_template <- tempfile(tmpdir = temp_data_output_dir, fileext = ".tex")
    new_set_background_images_template <- file.path(temp_data_output_dir, "updated_set_background_images.tex")
    writeLines(set_bg_images_lines, new_set_background_images_template)
    rm(set_bg_images_lines)
  }

  # ----
  # substitute values into placeholders
  #
  # All options in the YAML header cannot have parameters passed to them (ex. mainfont)
  # so we had to revert to use "placeholders" (ex. {mainfont}). To insert values into
  # the YAML header like the font or the file paths to the necessary files like the
  # references file we first read the lines of the quarto document, then gsub the
  # values into their corresponding placeholders and finally, write the new lines
  # back to a new file that's then used in quarto_render()
  # ----
  quarto_report <- readLines(quarto_report_template)
  text_font_size <- paste0(text_font_size, "pt")

  quarto_report <- gsub("\\{fontsize\\}", text_font_size, quarto_report)
  quarto_report <- gsub("\\{mainfont\\}", font_style, quarto_report)
  quarto_report <- gsub("\\{sansfont\\}", font_style, quarto_report)
  quarto_report <- gsub("\\{background_images\\}",
                          ifelse(output_format == "pdf", new_set_background_images_template, set_background_images_template),
                          quarto_report)

  extra_textual_content_quarto_template <- gsub("\\\\", "/", extra_textual_content_quarto_template)
  quarto_report <- gsub("\\{extra_textual_content\\}", extra_textual_content_quarto_template, quarto_report)

  references <- gsub("\\\\", "/", references)
  quarto_report <- gsub("\\{references\\}", references, quarto_report)

  word_template <- gsub("\\\\", "/", word_template)
  quarto_report <- gsub("\\{word_template\\}", word_template, quarto_report)

  citation_style <- gsub("\\\\", "/", citation_style)
  quarto_report <- gsub("\\{citation_style\\}", citation_style, quarto_report)

  updated_quarto_report <- tempfile(tmpdir = temp_data_output_dir, fileext = ".qmd")
  writeLines(quarto_report, updated_quarto_report)
  rm(quarto_report)


  # generate report elements

  #----
  # generate_element_paths
  #
  # save elements in an rds file to be passed to the Quarto file
  # This must be done as Quarto doesn't recognize R specific types
  #----
  generate_element_paths <- function(element){
    # save element into an rds file temporarily
    path <- tempfile(tmpdir = temp_data_output_dir, fileext = ".rds")
    saveRDS(element, path)
    return(path)
  }

  #----
  # The definitions and abbreviations get output with LaTeX when output_format = "pdf".
  # If we used a table, the table numbers tend to output incorrectly in the report
  # even without assigning these tables a label. Therefore, the solution was
  # to generate the list of items using LaTeX, solving this issue.
  # A flextable is output when output_format = "docx" because in Word, the
  # above issue does not arise.
  # To make this possible, a function needed to be passed to the Quarto file to generate
  # the correct output based on the output_format.
  # The function needs to be supplied the flextable for docx output and the raw data
  # for pdf output therefore, the data (table or raw) also needed to be passed to the
  # Quarto file in order for it to be available to the function.
  # Hence, two data paths get created for both 'definitions' and 'abbreviations':
  # one for the function and one for the data.
  #----

  #----
  # generate_element_based_on_output_format
  #
  # If output_format = "docx" a flextable is output.
  # If output_format = "pdf", LaTeX commands are output to generate a list of items and descriptions.
  #----
  generate_element_based_on_output_format <- function(data, output_format){
    if (output_format == "docx"){
      if (!("flextable" %in% class(data))){
        stop("Invalid input: data. data must be a flextable when output_format = 'docx'")
      }
      data
    } else {
      if (!is.data.frame(data)){
        stop("Invalid input: data. data must be data frame when output_format = 'pdf'")
      }
      cat("\\begin{description}")
      for (i in 1:nrow(data)){
        cat("\\item[", data[[1]][i], ":]")
        cat(data[[2]][i])
      }
      cat("\\end{description}")
    }
  }

  listed_elements_generator_function_path <- NULL
  if (!is.null(definitions) | !is.null(abbreviations)){
    listed_elements_generator_function_path <- generate_element_paths(generate_element_based_on_output_format)
  }

  # definitions
  definitions_data_path <- NULL
  if (!is.null(definitions)){
    if (output_format == "docx"){
      definitions_table <- two_column_table(
        data = definitions_data,
        output_format = output_format,
        font_size = table_font_size,
        font_style = font_style,
        display_headers = definitions_display_header)
      definitions_data_path <- generate_element_paths(definitions_table)
    } else {
      definitions_data_path <- generate_element_paths(definitions_data)
    }
  }

  # abbreviations
  abbreviations_data_path <- NULL
  if (!is.null(abbreviations)){
    if (output_format == "docx"){
      abbrev_table <- two_column_table(
        data = abbreviations_data,
        output_format = output_format,
        font_size = table_font_size,
        font_style = font_style,
        display_headers = abbreviations_display_header)
      abbreviations_data_path <- generate_element_paths(abbrev_table)
    } else {
      abbreviations_data_path <- generate_element_paths(abbreviations_data)
    }
  }

  linked_data_repr_tbl <- linkage_rate_table(
    main_data = main_data,
    output_format = output_format,
    column_var = stratified_linkage_tbls_column_var,
    strata_vars = linked_data_representativeness_tbl_strata_vars,
    display_total_column = TRUE,
    display_unlinked_column = FALSE,
    continuous_stat = stratified_linkage_tbls_continuous_stat,
    percent_type = "column",
    font_size = table_font_size,
    font_style = font_style,
    footnotes = linked_data_representativeness_tbl_footnotes,
    thousands_separator = thousands_separator,
    decimal_mark = decimal_mark,
    num_decimal_places = num_decimal_places,
    display_percent_symbol = display_percent_symbol,
    output_to_csv = stratified_linkage_tbls_output_to_csv,
    output_dir = output_dir
  )
  linked_data_repr_tbl_path <- generate_element_paths(linked_data_repr_tbl)

  # linkage rate table
  linkage_rate_tbl <- linkage_rate_table(
    main_data = main_data,
    output_format = output_format,
    column_var = stratified_linkage_tbls_column_var,
    strata_vars = linkage_rate_tbl_strata_vars,
    missing_data_indicators = missing_data_indicators,
    display_total_column = FALSE,
    display_unlinked_column = TRUE,
    continuous_stat = stratified_linkage_tbls_continuous_stat,
    percent_type = "row",
    font_size = table_font_size,
    font_style = font_style,
    footnotes = linkage_rate_tbl_footnotes,
    thousands_separator = thousands_separator,
    decimal_mark = decimal_mark,
    num_decimal_places = num_decimal_places,
    display_percent_symbol = display_percent_symbol,
    output_to_csv = stratified_linkage_tbls_output_to_csv,
    output_dir = output_dir
  )
  linkage_rate_table_path <- generate_element_paths(linkage_rate_tbl)

  # linkage rates over time plot
  linkage_rates_plot_path <- NULL
  if(!is.null(acquisition_year) & !is.null(acquisition_month)){
    linkage_rates_plot <- linkage_rates_over_time_plot(
      data = main_data,
      link_indicator_var = stratified_linkage_tbls_column_var,
      acquisition_year_var = acquisition_year_var,
      acquisition_month_var = acquisition_month_var)
    if (!is.null(linkage_rates_plot)){
      linkage_rates_plot_path <- generate_element_paths(linkage_rates_plot)
    }
  }

  # algorithm summary
  algorithm_summary_table_path <- NULL
  if (!is.null(algorithm_summary_data)) {
    alg_summ_tbl <- algorithm_summary_table(
      data = algorithm_summary_data,
      output_format = output_format,
      font_size = table_font_size,
      font_style = font_style,
      footnotes = algorithm_summary_tbl_footnotes,
      thousands_separator = thousands_separator,
      decimal_mark = decimal_mark,
      num_decimal_places = num_decimal_places
    )
    algorithm_summary_table_path <- generate_element_paths(alg_summ_tbl)
  }

  # performance measures table
  performance_measures_table_path <- NULL
  performance_measures_plot_path <- NULL
  performance_measures_plot_caption <- NULL
  if (!is.null(performance_measures_data)) {
    perf_meas_tbl <- performance_measures_table(
      data = performance_measures_data,
      ground_truth = ground_truth,
      output_format = output_format,
      font_size = table_font_size,
      font_style = font_style,
      footnotes = performance_measures_tbl_footnotes,
      thousands_separator = thousands_separator,
      decimal_mark = decimal_mark,
      num_decimal_places = num_decimal_places,
      num_record_pairs = num_pairs_non_missing_ground_truth,
      percent_record_pairs = percent_non_missing_ground_truth
    )
    performance_measures_table_path <- generate_element_paths(perf_meas_tbl)

    # performance measures plot
    performance_measures_plot_path <- tempfile(tmpdir = temp_data_output_dir,
                                               fileext = ifelse(output_format == "pdf", ".pdf", ".png"))
    if (output_format == "pdf"){
      pdf(performance_measures_plot_path, width = 5, height = 5)
      perf_meas_plot <- performance_measures_plot(performance_measures_data)
      dev.off()
    } else {
      png(performance_measures_plot_path, units = "in", width = 5, height = 5,
          res = 350)
      perf_meas_plot <- performance_measures_plot(performance_measures_data)
      dev.off()
    }
    if (length(perf_meas_plot) > 0){
      unlink(performance_measures_plot_path)
      performance_measures_plot_path <- NULL
    }

    concatenated_footnotes <- paste0(performance_measures_tbl_footnotes, collapse = " ")
    performance_measures_plot_caption <- paste0(
      "Radar chart showing classification performance for linking records in ",
      left_dataset_name,
      " to those in ",
      right_dataset_name,
      ". Classification performance was estimated among record pairs with non-missing values for ",
      ground_truth,
      ifelse(!is.null(num_pairs_non_missing_ground_truth) & !is.null(percent_non_missing_ground_truth),
             paste0(" (N = ", num_pairs_non_missing_ground_truth, ", ", percent_non_missing_ground_truth, "%)"),
             ""),
      " and reported as percentages (%). ",
      concatenated_footnotes)
  }

  # missingness table
  missingness_table_path <- NULL
  if (display_missingness_table){
    missingness_tbl <- missingness_table(
      data = missing_data_indicators,
      output_format = output_format,
      font_size = table_font_size,
      font_style = font_style,
      footnotes = missingness_tbl_footnotes,
      thousands_separator = thousands_separator,
      decimal_mark = decimal_mark,
      num_decimal_places = num_decimal_places,
      display_percent_symbol = display_percent_symbol
    )
    missingness_table_path <- generate_element_paths(missingness_tbl)
  }

  # save linkage rate
  if (save_linkage_rate){
    save_linkage_rate_sqlite_file(output_dir,
                                  report_generation_date,
                                  data_linker,
                                  left_dataset_name,
                                  right_dataset_name,
                                  overall_linkage_rate,
                                  data_time_period,
                                  project_id)
  }

  quarto_render(
  input = updated_quarto_report,
  output_format = output_format,
  execute_params = list(
  definitions_data_path = definitions_data_path,
  abbreviations_data_path = abbreviations_data_path,
  listed_elements_generator_function_path = listed_elements_generator_function_path,
  linked_data_repr_table_path = linked_data_repr_tbl_path,
  linkage_rate_table_path = linkage_rate_table_path,
  linkage_rates_plot_path = linkage_rates_plot_path,
  algorithm_summary_table_path = algorithm_summary_table_path,
  performance_measures_table_path = performance_measures_table_path,
  performance_measures_plot_path = performance_measures_plot_path,
  performance_measures_plot_caption = performance_measures_plot_caption,
  missingness_table_path = missingness_table_path,
  report_title = report_title,
  report_subtitle = report_subtitle,
  left_dataset_name = left_dataset_name,
  right_dataset_name = right_dataset_name,
  data_linker = data_linker,
  output_format = output_format,
  project_id = project_id,
  num_records_left_dataset = num_records_left_dataset,
  num_records_right_dataset = num_records_right_dataset,
  ground_truth = ground_truth,
  data_time_period = data_time_period,
  num_records_linked = num_records_linked,
  overall_linkage_rate = overall_linkage_rate,
  report_generation_date = report_generation_date,
  display_back_cover_page = display_back_cover_page,
  comprehensive_report = comprehensive_report,
  linkage_package = linkage_package,
  linkage_package_version = linkage_package_version,
  linkrep_package_version = linkrep_package_version,
  R_version = R_version,
  datastan_package_version = datastan_package_version
  ))

  # Format final output:

  # change file name and location
  # file automatically saves in calling location therefore need to manually move it to output_dir
  file_name <- paste0("Record Linkage Quality Report.", output_format)
  src <- gsub("qmd", output_format, updated_quarto_report)
  result <- file.rename(src, paste0(output_dir, "/", file_name))
  message(paste0("Ignore above, output created: ", file_name))

  # delete extra files that were created
  unlink(updated_quarto_report)
  if (!is.null(new_set_background_images_template)){
    unlink(new_set_background_images_template)
  }
  if (!is.null(definitions_data_path)){
    unlink(definitions_data_path)
  }
  if (!is.null(abbreviations_data_path)){
    unlink(abbreviations_data_path)
  }
  if (!is.null(listed_elements_generator_function_path)){
    unlink(listed_elements_generator_function_path)
  }
  unlink(linkage_rate_table_path)
  if (!is.null(linkage_rates_plot_path)){
    unlink(linkage_rates_plot_path)
  }
  if (!is.null(algorithm_summary_table_path)){
    unlink(algorithm_summary_table_path)
  }
  if (!is.null(performance_measures_table_path)){
    unlink(performance_measures_table_path)
  }
  if (!is.null(performance_measures_plot_path)){
    unlink(performance_measures_plot_path)
  }
  if (!is.null(missingness_table_path)){
    unlink(missingness_table_path)
  }
}



