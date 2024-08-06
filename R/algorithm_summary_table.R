#' Generate Algorithm Summary Table
#'
#' Generates a formatted flextable specific to the algorithm summary table.
#'
#' @param data A data frame that data contains information on the linkage algorithm
#'  (ex. linking variables on each pass).
#' @param output_formatString specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for
#' the table. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#'
#' @return A \code{flextable}.
#' @export
#'
algorithm_summary_table <- function(data,
                                    output_format,
                                    font_size = 12,
                                    font_style = "Times New Roman",
                                    footnotes = NULL,
                                    thousands_separator = ",",
                                    decimal_mark = ".",
                                    num_decimal_places = 1
){
  HEADER_ALIGNMENT <- "center"
  BODY_ALIGNMENT <- "center"

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
