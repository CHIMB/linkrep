#' Generate Algorithm Summary Table
#'
#' Generates a formatted flextable specific to the algorithm summary table.
#'
#' @param data A data frame.
#' @param output_format A character string specifying the output format, must be
#'  one of "`pdf`" or "`docx`".
#' @param font_size A numeric specifying the font size for the table text.
#' @param font_style A character string specifying the font. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#' @param footnotes A character vector of additional footnotes. Each element in
#'  the vector will be displayed on a new line.
#' @param thousands_separator A character string specifying the style of the
#'  thousands separator. Default is "`,`".
#' @param decimal_mark A character string specifying the style of the decimal mark.
#'  Default is "`.`".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark. Default is `1`.
#'
#' @return A `flextable`.
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
