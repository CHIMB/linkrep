#' Generate Performance Measures Table
#'
#' Generates a formatted flextable specific to the performance measures table.
#'
#' @param data A data frame that contains performance measures
#'  (ex. classification metrics such as sensitivity/recall). Data must be in
#'  percentages with values between 0 and 100 when using the default quarto template.
#' @param ground_truth String indicating the ground truth used to produce the
#'  performance measures.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for the performance
#'  measures table. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#' @param num_record_pairs The number (formatted) of record pairs with non-missing ground truth.
#' @param percent_record_pairs The percent (formatted) of record pairs with non-missing ground truth.
#'
#' @return A \code{flextable}.
#'
#' @keywords internal
#' @noRd
#'
performance_measures_table <- function(data,
                                       ground_truth,
                                       output_format,
                                       font_size = 12,
                                       font_style = "Times New Roman",
                                       footnotes = NULL,
                                       thousands_separator = ",",
                                       decimal_mark = ".",
                                       num_decimal_places = 1,
                                       num_record_pairs = NULL,
                                       percent_record_pairs = NULL
){
  HEADER_ALIGNMENT <- "center"
  BODY_ALIGNMENT <- "center"

  # add extra footnote to end of provided footnotes
  footnotes <- append(footnotes, paste0("Classification performance was estimated among record pairs with non-missing values for ",
                                        ground_truth,
                                        ifelse(!is.null(num_record_pairs) & !is.null(percent_record_pairs), paste0(" (N = ", num_record_pairs, ", ", percent_record_pairs, "%)"), ""),
                                        " and reported as percentages (%)."))

  # generate the table
  table <- formatted_flextable(data,
                               output_format,
                               font_size,
                               font_style,
                               footnotes,
                               thousands_separator,
                               decimal_mark,
                               num_decimal_places,
                               HEADER_ALIGNMENT,
                               BODY_ALIGNMENT)

  return(table)
}

