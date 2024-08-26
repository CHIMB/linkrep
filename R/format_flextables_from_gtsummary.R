#' Format Flextable
#'
#' Formats the provided flextable with the modifications.
#'
#' @param flextable A \code{flextable} object.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for
#' the table. Each element in the vector will be displayed on a new line.
#'
#' @return A \code{flextable} object: the result of the modifications applied to the
#'  \code{flextable} input.
#'
#' @import flextable
#'
#' @keywords internal
#' @noRd
#'
format_flextables_from_gtsummary <- function(flextable,
                                             output_format,
                                             font_size,
                                             font_style,
                                             footnotes = NULL){
  validate_flextable(flextable, "flextable")
  validate_common_parameters(output_format = output_format,
                             font_size = font_size,
                             font_style = font_style)

  if (!is.null(footnotes)){
    validate_string_vector(footnotes, "footnotes")
    flextable <- add_footer_lines(flextable, values = footnotes)
  }

  flextable <- fontsize(flextable, size = font_size, part = "all")
  flextable <- font(flextable, fontname = font_style, part = "all")

  if (output_format == "pdf"){
    flextable <- set_table_width(flextable)
  }

  return(flextable)
}
