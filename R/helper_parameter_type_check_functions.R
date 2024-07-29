#' Validate Boolean Parameters
#'
#' Check if parameter is a boolean (logical) value.
#'
#' @param param The boolean parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
validate_boolean <- function(param, param_name){
  if(is.null(param) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be TRUE or FALSE", param_name, param_name))
  }
  if (is.na(param) | !is.logical(param)){
    stop(sprintf("Invalid argument: %s. %s must be TRUE or FALSE", param_name, param_name))
  }
}


#' Validate String Parameters
#'
#' Check if parameter is a single character string.
#'
#' @param param The string parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
validate_string <- function(param, param_name){
  if (!is.character(param) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be a single character string", param_name, param_name))
  }
}


#' Validate Numeric Parameters
#'
#' Check if parameter is a numeric value.
#'
#' @param param The numeric parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
validate_number <- function(param, param_name){
  if ((!is.numeric(param) & !is.integer(param)) | length(param) != 1){
    stop(sprintf("Invalid argument: %s. %s must be a single numeric value", param_name, param_name))
  }
}


#' Validate String Vector Parameters
#'
#' Check if parameter is a string vector.
#'
#' @param param The string vector parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
validate_string_vector <- function(param, param_name){
  if (!is.character(param)) {
    stop(sprintf("Invalid argument: %s. %s must be a character string vector", param_name, param_name))
  }
}


#' Validate Dataframe Parameters
#'
#' Check if parameter is a `data.frame` or `data.table` object and make sure it's not empty.
#'
#' @param param The dataframe parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
#' @importFrom data.table is.data.table data.table
#'
validate_df <- function(param, param_name){
  if (!is.data.frame(param) & !is.data.table(param)) {
    stop(sprintf("Invalid argument: %s. %s must be a dataframe", param_name, param_name))
  }
  if (nrow(param) == 0){
    stop(sprintf("Invalid argument: %s. %s cannot be empty", param_name, param_name))
  }
}


#' Validate Variable is in Data
#'
#' Check if a variable name is present in a dataframe.
#'
#' @param var The name of the variable to check its presence `data`.
#' @param data The dataframe that should contain the variable name passed in.
#' @param var_param_name The name of the variable parameter to be used in potential error messages.
#' @param data_param_name The name of the data parameter to be used in potential error messages.
#'
validate_var_in_data <- function(var, data, var_param_name, data_param_name){
  if (!(var %in% names(data))){
    stop(sprintf("Invalid argument: %s. '%s' must be present in '%s'", var_param_name, var, data_param_name))
  }
}


#' Validate Dataframe is Binary or Logical
#'
#' Check if all the variables in a data frame are binary or logical
#'
#' @param param The dataframe parameter to check.
#' @param param_name The name of the parameter to be used in potential error messages.
#'
validate_df_binary <- function(param, param_name){
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

