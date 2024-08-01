#' Generate Abbreviation Table
#'
#' Generates a formatted flextable specific to the abbreviation table.
#'
#' @param data A data frame.
#' @param output_format A character string specifying the output format, must be
#'  one of "`pdf`" or "`docx`".
#' @param font_size A numeric specifying the font size for the table text.
#' @param font_style A character string specifying the font. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#' @param display_headers A logical indicating whether to display the headers in
#'  the table. Default is `TRUE`.
#'
#' @return A `flextable`.
#'
#' @importFrom flextable border_remove delete_part
#'
abbreviation_table <- function(data,
                               output_format,
                               font_size = 12,
                               font_style = "Times New Roman",
                               display_headers = TRUE){
  HEADER_ALIGNMENT <- "left"
  BODY_ALIGNMENT <- "left"

  # the two functions below handle all the input errors

  # generate the table
  table <- formatted_flextable(data,
                               output_format,
                               font_size,
                               font_style,
                               header_align = HEADER_ALIGNMENT,
                               body_align = BODY_ALIGNMENT)

  validate_boolean(display_headers, "display_headers")

  if (!display_headers){
    table <- border_remove(table)
    table <- delete_part(table, part = "header")
  }

  return(table)
}
