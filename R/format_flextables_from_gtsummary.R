#' Format Flextable
#'
#' Formats the provided flextable with the modifications.
#'
#' @param flextable A `flextable` object.
#' @param output_format A character string specifying the output format, must be
#'  one of "`pdf`" or "`docx`".
#' @param font_size A numeric specifying the font size for the table text.
#' @param font_style A character string specifying the font. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#' @param footnotes A character string of footnotes. Must be one string when called with this function.
#'
#' @return A `flextable`.
#'
#' @import flextable
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
    validate_string(footnotes, "footnotes")
    flextable <- add_footer_lines(flextable, values = footnotes)
  }

  flextable <- fontsize(flextable, size = font_size, part = "all")
  flextable <- font(flextable, fontname = font_style, part = "all")

  if (output_format == "pdf"){
    flextable <- set_table_width(flextable)
  }

  return(flextable)
}
