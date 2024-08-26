#' Validate String Parameters
#'
#' Check if parameter is a single character string.
#'
#' @param param The string parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_string <- function(param, param_name){
  if (!is.character(param_name) | length(param_name) != 1){
    stop("Invalid argument: param_name. param_name must be a single string")
  }
  if (!is.character(param) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be a single string", param_name, param_name))
  }
}


#' Validate String Vector Parameters
#'
#' Check if parameter is a string vector.
#'
#' @param param The string vector parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_string_vector <- function(param, param_name){
  if (!is.character(param)) {
    stop(sprintf("Invalid argument: %s. %s must be a character string vector", param_name, param_name))
  }
}


#' Validate Boolean Parameters
#'
#' Check if parameter is a boolean (logical) value.
#'
#' @param param The boolean parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_boolean <- function(param, param_name){
  if(is.null(param) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be TRUE or FALSE", param_name, param_name))
  }
  if (is.na(param) | !is.logical(param)){
    stop(sprintf("Invalid argument: %s. %s must be TRUE or FALSE", param_name, param_name))
  }
}


#' Validate Numeric Parameters
#'
#' Check if parameter is a numeric value.
#'
#' @param param The numeric parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_numeric <- function(param, param_name){
  validate_string(param_name, "param_name")
  if ((!is.numeric(param) & !is.integer(param)) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be a single numeric value", param_name, param_name))
  }
}


#' Validate Numeric Vector Parameters
#'
#' Check if parameter is a numeric vector.
#'
#' @param param The numeric parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_numeric_vector <- function(param, param_name){
  validate_string(param_name, "param_name")
  if ((!is.numeric(param) & !is.integer(param))){
    stop(sprintf("Invalid argument: %s. %s must be a numeric vector", param_name, param_name))
  }
}



#' Validate Data Frame Parameters
#'
#' Check if parameter is a `data.frame` or `data.table` object and make sure it's not empty.
#'
#' @param param The data frame parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @importFrom data.table is.data.table data.table
#'
#' @keywords internal
#' @noRd
#'
validate_df <- function(param, param_name){
  validate_string(param_name, "param_name")
  if (!is.data.frame(param) & !is.data.table(param)) {
    stop(sprintf("Invalid argument: %s. %s must be a dataframe", param_name, param_name))
  }
  if (nrow(param) == 0){
    stop(sprintf("Invalid argument: %s. %s cannot be empty", param_name, param_name))
  }
}


#' Validate a Variable is Present in the Data
#'
#' Check if a variable name is present in a data frame.
#'
#' @param var The name of the variable to check its presence `data`.
#' @param data The dataframe that should contain the variable name passed in.
#' @param var_param_name The name of the variable parameter to be used in potential error messages.
#' @param data_param_name The name of the data parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_var_in_data <- function(var, data, var_param_name, data_param_name){
  validate_string(var, "var")
  validate_df(data, "data")
  validate_string(var_param_name, "var_param_name")
  validate_string(data_param_name, "data_param_name")
  if (!(var %in% names(data))){
    stop(sprintf("Invalid argument: %s. '%s' must be present in '%s'", var_param_name, var, data_param_name))
  }
}


#' Validate Data Frame is Binary or Logical
#'
#' Check if all the variables in a data frame are binary or logical
#'
#' @param param The data frame parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_df_binary <- function(param, param_name){
  validate_df(param, "param")
  validate_string(param_name, "param_name")
  #----
  # check that the variables are all binary.
  # Logical values automatically convert to 0 (FALSE) and 1 (TRUE) so works for logical variables as well.
  #----
  is_binary <- function(var){
    sum(var != 0 & var != 1) == 0 & sum(is.na(var)) == 0
  }
  binary <- sapply(param, is_binary)
  if (FALSE %in% unique(binary)){
    stop(sprintf("Invalid argument: %s. All variables must be binary or logical", param_name))
  }
}


#' Validate Parameter is a `flextable`
#'
#' @param ft The `flextable` to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @keywords internal
#' @noRd
#'
validate_flextable <- function(ft, param_name){
  validate_string(param_name, "param_name")
  if (!('flextable' %in% unique(class(ft)))){
    stop(sprintf("Invalid argument: %s. %s must be a flextable object", param_name, param_name))
  }
}


#' Validate Common Parameters
#'
#' Check if the parameters listed are input correctly.
#'
#' @param data A data frame.
#' @param footnotes A character vector.
#' @param output_format One of "`pdf`" or "`docx`".
#' @param thousands_separator A string.
#' @param decimal_mark A string.
#' @param num_decimal_places A number \eqn{>= 0}.
#' @param display_percent_symbol A logical.
#' @param font_size A number > 0.
#' @param font_style A string present in \code{system_fonts()$name} or
#'  \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#'
#' @importFrom systemfonts system_fonts
#'
#' @keywords internal
#' @noRd
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
    validate_string(output_format, "output_format")
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
    validate_numeric(num_decimal_places, "num_decimal_places")
    if (num_decimal_places < 0){
      stop("Invalid argument: num_decimal_places must be >= 0.")
    }
  }

  if (!is.null(display_percent_symbol)){
    validate_boolean(display_percent_symbol, "display_percent_symbol")
  }

  if (!is.null(font_size)){
    validate_numeric(font_size, "font_size")
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

