#' Generate a Two Column Table
#'
#' Generates a formatted flextable.
#'
#' @param data A data frame. Data must contain two columns.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param display_headers A logical indicating whether to display the
#'  column headers in the abbreviation table. Only applied when \code{output_format =}
#'  "\code{docx}".
#'
#' @return A \code{flextable} object.
#'
#' @importFrom flextable border_remove delete_part
#'
#' @keywords internal
#' @noRd
#'
two_column_table <- function(data,
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
  if (ncol(data) != 2){
    stop("Invalid argument: data. data must have two columns")
  }

  validate_boolean(display_headers, "display_headers")

  if (!display_headers){
    table <- border_remove(table)
    table <- delete_part(table, part = "header")
  }

  return(table)
}
