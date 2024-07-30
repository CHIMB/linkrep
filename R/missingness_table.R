#' Generate Missingness Table
#'
#' Generates a table stratified by variables representing the missingness of the data.
#'
#' @param data A binary/logical data frame.
#' @param output_format A character string specifying the output format, must be one of "`pdf`" or "`docx`".
#' @param font_size A numeric specifying the font size for the table text.
#' @param font_style A character string specifying the font. Must be present in \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}.
#' @param footnotes A character vector of additional footnotes. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A character string specifying the style of the thousands separator. Default is "`,`".
#' @param decimal_mark A character string specifying the style of the decimal mark. Default is "`.`".
#' @param num_decimal_places A number specifying the number of digits to output after the decimal mark. Default is `1`.
#' @param display_percent_symbol A logical indicating whether you want a percent symbol to display in the table. Default is `FALSE`.
#'
#' @return A `flextable` that was originally a `gtsummary`.
#'
#' @importFrom gtsummary theme_gtsummary_language tbl_summary as_flex_table modify_header modify_footnote
#'
missingness_table <- function(data,
                              output_format,
                              font_size = 12,
                              font_style = "Times New Roman",
                              footnotes = NULL,
                              thousands_separator = ",",
                              decimal_mark = ".",
                              num_decimal_places = 1,
                              display_percent_symbol = FALSE
)
{
  # handle input errors
  validate_common_parameters(data = data,
                             footnotes = footnotes,
                             output_format = output_format,
                             thousands_separator = thousands_separator,
                             decimal_mark = decimal_mark,
                             num_decimal_places = num_decimal_places,
                             display_percent_symbol = display_percent_symbol,
                             font_size = font_size,
                             font_style = font_style)
  validate_df_binary(data, "data")

  theme_gtsummary_language("en", decimal_mark, thousands_separator)

  stat <- ifelse(display_percent_symbol, "{n} ({p}%)", "{n} ({p})")

  # generate missingness table
  table <- tbl_summary(
    data,
    statistic = all_categorical() ~ stat,
    digits = everything() ~ c(0, num_decimal_places),
    missing = "no"
  )

  table <- modify_header(
    table,
    label = "**Variable**",
    stat_0 ~ "**Missing**\n(N = {style_number(N)})")

  table <- modify_footnote(
    table,
    update = all_stat_cols() ~ NA
  )

  # transform gtsummary table into a flextable to ensure consistency in report output
  table <- as_flex_table(table)

  if (is.null(footnotes)) {
    footnotes <- "Data are presented as n (%)."
  } else {
    footnotes <- paste(footnotes, collapse = "\n")
    footnotes <- paste0(footnotes, "\n", "Data are presented as n (%).")
  }

  table <- format_flextables_from_gtsummary(table,
                                            output_format,
                                            font_size,
                                            font_style,
                                            footnotes)

  return(table)
}
