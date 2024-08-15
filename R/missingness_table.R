#' Generate Missingness Table
#'
#' Generates a table stratified by variables representing the missingness of the data.
#'
#' @param data A data frame. All variables must be logical or binary, with
#' \code{1} or \code{TRUE} representing a missing record for that variable.
#'  conventions and where this data is used in the report.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for
#' the missigness table. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#' @param display_percent_symbol A logical indicating whether to display a percent symbol
#' next to percentages table. Default is \code{FALSE}.
#'
#' @return A \code{flextable} that was originally a \code{gtsummary}.
#'
#' @importFrom gtsummary theme_gtsummary_language tbl_summary as_flex_table modify_header modify_footnote all_categorical everything all_stat_cols
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
