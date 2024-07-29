#' Validate Common Parameters
#'
#' Check if the parameters listed are input correctly.
#'
#' @param data A dataframe.
#' @param footnotes A character vector.
#' @param output_format One of "`pdf`" or "`docx`".
#' @param thousands_separator A string.
#' @param decimal_mark A string.
#' @param num_decimal_places A number \eqn{>= 0}.
#' @param display_percent_symbol A logical.
#' @param font_size A number > 0.
#' @param font_style A string present in \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#'
#' @importFrom systemfonts system_fonts
#'
validate_common_parameters <- function(data = NULL,
                                       footnotes = NULL,
                                       output_format = NULL,
                                       thousands_separator = NULL,
                                       decimal_mark = NULL,
                                       num_decimal_places = NULL,
                                       display_percent_symbol = NULL,
                                       font_size = NULL,
                                       font_style = NULL
){

  if (!is.null(data)){
    validate_df(data, "data")
  }

  if (!is.null(footnotes)){
    validate_string_vector(footnotes, "footnotes")
  }

  if (!is.null(output_format)){
    if (length(output_format) != 1) {
      stop("Invalid argument: output_format. Options: 'pdf' or 'docx'")
    }
    if (output_format != "pdf" & output_format != "docx"){
      stop("Invalid argument: output_format. Options: 'pdf' or 'docx'")
    }
  }

  if (!is.null(thousands_separator)){
    validate_string(thousands_separator, "thousands_separator")
  }

  if (!is.null(decimal_mark)){
    validate_string(decimal_mark, "decimal_mark")
  }

  if (!is.null(num_decimal_places)){
    validate_number(num_decimal_places, "num_decimal_places")
    if (num_decimal_places < 0){
      stop("Invalid argument: num_decimal_places must be >= 0.")
    }
  }

  if (!is.null(display_percent_symbol)){
    validate_boolean(display_percent_symbol, "display_percent_symbol")
  }

  if (!is.null(font_size)){
    validate_number(font_size, "font_size")
    if (font_size <= 0){
      stop("Invalid argument: font_size. font_size must be > 0.")
    }
  }

  if (!is.null(font_style)){
    validate_string(font_style, "font_style")
    if (!(font_style %in% system_fonts()$family | font_style %in% system_fonts()$name)){
      stop("Invalid argument: font_style. font_style must be a valid font found in system_fonts().")
    }
  }
}
