#' Generate Linkage Rate Table
#'
#' Generates a summary table of linkage rates stratified by characteristics.
#'
#' @param main_data A data frame containing the variables present in the left dataset
#' of the linkage.
#' @param output_format String specifying the desired output format. Allowed values
#'  are "\code{pdf}" or "\code{docx}".
#' @param column_var A string of the name of a logical or binary variable
#'  present in \code{main_data} that indicates whether a record linked or not.
#'  Its values will be the columns in the table.
#' @param strata_vars A character vector of the names of the variables present in
#'  \code{main_data} to stratify the table by.
#' @param missing_data_indicators A data frame. All variables in the data
#'  must be logical or binary, with \code{1} or \code{TRUE} representing a missing
#'  record for that variable. See Details section for more information on naming
#'  conventions and how this data is used in the table.
#' @param display_total_column A logical indicating whether to
#'  display a total (overall) column in the table. Default is \code{TRUE}.
#' @param display_unlinked_column A logical indicating whether to
#'  display the unlinked column in the table. Default is \code{TRUE}.
#' @param continuous_stat A string indicating which statistic to use on continuous
#'  variables. Allowed values are "\code{mean}" or "\code{median}" (default). If
#'  "\code{mean}", mean \eqn{\pm} standard deviation will be output otherwise,
#'  median (Q1, Q3), where Q1 is the 25\eqn{^{th}} percentile, and Q3 is the
#'  75\eqn{^{th}} percentile, will be output.
#' @param percent_type String specifying the desired percent type. Allowed values
#'  are "\code{row}" or "\code{column}".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for
#' the linkage rate table. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#' @param display_percent_symbol A logical indicating whether to display a percent symbol
#' next to percentages table. Default is \code{FALSE}.
#' @param output_to_csv A logical indicating whether to save the  table in a csv
#' file. Default is \code{FALSE}.
#' @param output_dir A path to a directory. The csv file containing the
#'  table will be saved here if `output_to_csv = TRUE`.
#' @param threshold A small count threshold that may be supplied by the user if they wish small counts below this threshold
#'  to be suppressed in the output of the linkage quality report.
#'
#' @details
#' Details on \code{missing_data_indicators}:\cr
#' Variables associated with those in \code{main_data} must either have the same
#'  variable name suffixed by "\code{_missing}" or have the same label for it to be
#'  displayed in the linkage rate table as a value of that variable. In this case,
#'  the variable will be relabelled "\code{Missing}" and tabbed under the header of
#'  the variable it's associated with. If the variable is not associated with one
#'  in \code{main_data} it will be relabelled with its label or variable name
#'  prefixed by "\code{Missing }".
#'
#' @return A \code{flextable} that was originally a \code{gtsummary}.
#'
#' @importFrom gtsummary theme_gtsummary_language tbl_summary as_flex_table modify_header modify_footnote add_overall bold_labels style_number all_categorical all_continuous all_stat_cols
#' @importFrom dplyr select mutate relocate across where
#' @importFrom tidyselect all_of
#' @importFrom Hmisc label
#' @importFrom rlang := !!
#' @importFrom utils write.csv
#'
#' @keywords internal
#' @noRd
#'
linkage_rate_table <- function(main_data,
                               output_format,
                               column_var,
                               strata_vars,
                               missing_data_indicators = NULL,
                               display_total_column = TRUE,
                               display_unlinked_column = TRUE,
                               continuous_stat = "median",
                               percent_type = "row",
                               font_size = 12,
                               font_style = "Times New Roman",
                               footnotes = NULL,
                               thousands_separator = ",",
                               decimal_mark = ".",
                               num_decimal_places = 1,
                               display_percent_symbol = FALSE,
                               output_to_csv = FALSE,
                               output_dir = NULL,
                               threshold = NULL
)

{
  # handle input errors
  validate_common_parameters(data = main_data,
                             footnotes = footnotes,
                             output_format = output_format,
                             thousands_separator = thousands_separator,
                             decimal_mark = decimal_mark,
                             num_decimal_places = num_decimal_places,
                             display_percent_symbol = display_percent_symbol,
                             font_size = font_size,
                             font_style = font_style)

  validate_string(column_var, "column_var")
  validate_string_vector(strata_vars, "strata_vars")

  if (!is.null(missing_data_indicators)){
    validate_df(missing_data_indicators, "missing_data_indicators")
    validate_df_binary(missing_data_indicators, "missing_data_indicators")

    # need these to be dataframes to use dplyr manipulation below
    main_data <- as.data.frame(main_data)
    missing_data_indicators <- as.data.frame(missing_data_indicators)
  }

  validate_boolean(display_total_column, "display_total_column")
  validate_boolean(display_unlinked_column, "display_unlinked_column")
  validate_string(continuous_stat, "continuous_stat")
  if (continuous_stat != "median" & continuous_stat != "mean"){
    stop("Invalid argument: continuous_stat. Options: 'median' or 'mean'")
  }
  validate_string(percent_type, "percent_type")
  if (percent_type != "row" & percent_type != "column"){
    stop("Invalid argument: percent_type. Options: 'row' or 'column'")
  }
  validate_boolean(output_to_csv, "output_to_csv")

  if (!is.null(output_dir)){
    validate_string(output_dir, "output_dir")
    if (!dir.exists(output_dir)) {
      stop("Invalid argument: output_dir. output_dir must be a path to a directory")
    }
  }

  validate_var_in_data(column_var, main_data, "column_var", "main_data")
  if (sum(is.na(main_data[[column_var]])) > 0 |
      sum(main_data[[column_var]] != 0 & main_data[[column_var]] != 1) > 0){
    stop("Invalid argument: column_var must be a binary or logical variable in 'main_data'")
  }

  invalid_strata_vars <- base::setdiff(strata_vars, names(main_data))
  if (length(invalid_strata_vars) > 0) {
    stop("Invalid argument: strata_vars. Not all variables provided in strata_vars are present in 'main_data'")
  }
  if (length(strata_vars) == 1 & is.null(missing_data_indicators)){
    if (strata_vars == column_var){
      stop("column_var and strata_vars cannot be the same")
    }
  }

  if (output_to_csv & is.null(output_dir)){
    stop("output_dir must be provided when output_to_csv is TRUE")
  }

  theme_gtsummary_language("en", decimal_mark, thousands_separator)

  # data_subset will contain the necessary variables to generate the table
  data_subset <- select(main_data, all_of(strata_vars), all_of(column_var))
  # save labels to restore them after certain calculations remove them
  labels <- label(data_subset)

  # In all categorical variables, turn NA values into "Missing" so they are viewed
  # as a level in the table output and incorporated into the column percent calculations.
  data_subset <- mutate(data_subset, across(where(~ !is.numeric(.) & !is.integer(.)), ~ ifelse(is.na(.), "Missing",  if (is.factor(.)) as.character(.) else .)))

  # Add the missing value indicators to data_subset
  if (!is.null(missing_data_indicators)){
    # Match the columns in the two datasets and label the matched missing indicators "Missing"
    i <- 1
    while(i <= ncol(data_subset) & ncol(missing_data_indicators) > 0){
      # missing indicators labels
      missing_labels <- label(missing_data_indicators)

      data_subset_col_name <- names(data_subset)[i]
      # naming standard for missing field indicators
      missing_col_name <- paste0(data_subset_col_name, "_missing")

      # 1. The name of the variable in main_data_missing matches the name of the variable in main_data suffixed by '_missing'
      # 2. The label of the variable in main_data_missing matches the label of the variable in main_data
      if (missing_col_name %in% names(missing_data_indicators)) {
        missing_data_indicators[[missing_col_name]] <- NULL
      } else {
        col_label <- labels[[i]]
        if ((col_label != "") & (col_label %in% missing_labels)) {
          missing_index <- which(missing_labels == col_label)
          missing_col_name <- names(missing_labels)[missing_index]
          missing_data_indicators[[missing_col_name]] <- NULL
        }
      }
      if ("Missing" %in% unique(data_subset[[i]])) {
        # Get the unique levels excluding "Missing", and sort them alphabetically
        other_levels <- sort(setdiff(unique(data_subset[[i]]), "Missing"))
        # Reorder the column's levels so that "Missing" comes last
        data_subset[[i]] <- factor(data_subset[[i]], levels = c(other_levels, "Missing"))
      }
      # restore label
      if (labels[[i]] != ""){
        Hmisc::label(data_subset[[i]]) <- labels[[i]]
      }
      i <- i + 1
    }

    # label remaining missing indicators with "Missing " in front of the label or variable name so it stands on its own in the table
    if (ncol(missing_data_indicators) > 0) {
      for (i in seq_along(missing_data_indicators)){
        col_label <- label(missing_data_indicators[,i])
        if (col_label == ""){
          Hmisc::label(missing_data_indicators[,i]) <- paste("Missing", names(missing_data_indicators)[i])
        } else {
          Hmisc::label(missing_data_indicators[,i]) <- paste("Missing", col_label)
        }
      }
    }

    # combine the two datasets
    data_subset <- cbind(data_subset, missing_data_indicators)
  } else {
    for (i in names(data_subset)) {
      if ("Missing" %in% unique(data_subset[[i]])) {
        # Get the unique levels excluding "Missing", and sort them alphabetically
        other_levels <- sort(setdiff(unique(data_subset[[i]]), "Missing"))
        # Reorder the column's levels so that "Missing" comes last
        data_subset[[i]] <- factor(data_subset[[i]], levels = c(other_levels, "Missing"))
      }
      # restore label
      if (labels[[i]] != ""){
        Hmisc::label(data_subset[[i]]) <- labels[[i]]
      }
    }
  }

  categorical_stat <- ifelse(display_percent_symbol, "{n} ({p}%)", "{n} ({p})")
  continuous_stat <- ifelse(continuous_stat == "mean",
                            "{mean} \u00B1 {sd}",
                            "{median} ({p25}, {p75})")

  # identify factor levels used in column_var
  if ("logical" %in% unique(class(data_subset[[column_var]]))){
    factor_levels <- c(TRUE, FALSE)
  } else {
    factor_levels <- c(1,0)
  }

  # make table columns "Linked" and "Unlinked" by labeling the factors in column_var
  data_subset[[column_var]] <- factor(data_subset[[column_var]],
                                      levels = factor_levels,
                                      labels = c("Linked", "Unlinked"))


  # If the threshold is not NULL, then we can do these extra steps to remove any small counts
  removed_cols <- c()
  if(!is.null(threshold)){
    # Generate the linkage table using just integer values
    table <- tbl_summary(
      data_subset,
      by = all_of(column_var),
      statistic = list(
        all_categorical() ~ "{n}",
        all_continuous() ~ "{mean}"
      )
    )

    #print(table$table_body, n = 100)
    table_body <- table$table_body

    # Get the column names and labels
    col_names  <- colnames(main_data)
    col_labels <- label(main_data)

    # Variables and labels to remove
    variables_to_remove <- c()
    labels_to_remove <- c()

    # Modify specific rows to show "0 (0.0)" if no missing values
    for (i in 1:nrow(table_body)) {
      # Get the column and label for this iteration
      col_name  <- table_body$variable[i]
      col_label <- table_body$var_label[i]
      var_type  <- table_body$var_type[i]

      # If the variable type is continous, then skip it
      if(var_type != "continuous"){
        # Get the stat 1 and stat 2
        stat_1 <- table_body$stat_1[i]
        stat_2 <- table_body$stat_2[i]

        # Convert to numbers
        stat_1 <- suppressWarnings(as.numeric(gsub(",", "", stat_1)))
        stat_2 <- suppressWarnings(as.numeric(gsub(",", "", stat_2)))

        # Little function for checking if a number is whole or not
        is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

        # First, make sure the first and second value aren't NA, otherwise ignore the row
        if(!is.na(stat_1) && !is.na(stat_2)){
          # Secondly, make sure that the first and second value are integers
          if(is.wholenumber(stat_1) == TRUE && is.wholenumber(stat_2 == TRUE)){
            # Lastly, check if the value are within the thresholds
            if((stat_1 > 0 && stat_1 <= threshold) || (stat_2 > 0 && stat_2 <= threshold)){
              # Track the variable/label to remove
              variables_to_remove <- append(variables_to_remove, col_name)
              labels_to_remove <- append(labels_to_remove, col_label)

              # Keep unique names only
              variables_to_remove <- unique(variables_to_remove)
              labels_to_remove <- unique(labels_to_remove)
            }
          }
        }
      }
    }

    # Filter out rows where the variable or label is in the remove lists
    table$table_body <- table_body %>%
      filter(
        !(variable %in% variables_to_remove | var_label %in% labels_to_remove)
      )

    # Print the filtered table to verify
    #print(table$table_body, n = 100)

    # Track the removed columns
    removed_cols <- labels_to_remove

    # Filter data_subset before creating tbl_summary()
    if(length(variables_to_remove) > 0){
      data_subset_filtered <- data_subset %>%
        select(-all_of(variables_to_remove))

      # Otherwise, if a threshold was not provided, generate the linkage table normally
      table <- tbl_summary(
        data_subset_filtered,
        by = all_of(column_var),
        statistic = list(
          all_categorical() ~ categorical_stat,
          all_continuous() ~ continuous_stat
        ),
        digits = list(
          all_categorical() ~ c(0, num_decimal_places),
          all_continuous() ~ num_decimal_places
        ),
        percent = percent_type,
        missing = "ifany",
        missing_text = "Missing"
      )
    }
    else{
      # Otherwise, if a threshold was not provided, generate the linkage table normally
      table <- tbl_summary(
        data_subset,
        by = all_of(column_var),
        statistic = list(
          all_categorical() ~ categorical_stat,
          all_continuous() ~ continuous_stat
        ),
        digits = list(
          all_categorical() ~ c(0, num_decimal_places),
          all_continuous() ~ num_decimal_places
        ),
        percent = percent_type,
        missing = "ifany",
        missing_text = "Missing"
      )
    }
  }
  else{
    # Otherwise, if a threshold was not provided, generate the linkage table normally
    table <- tbl_summary(
      data_subset,
      by = all_of(column_var),
      statistic = list(
        all_categorical() ~ categorical_stat,
        all_continuous() ~ continuous_stat
      ),
      digits = list(
        all_categorical() ~ c(0, num_decimal_places),
        all_continuous() ~ num_decimal_places
      ),
      percent = percent_type,
      missing = "ifany",
      missing_text = "Missing"
    )
  }

  column_headers <- ifelse(display_unlinked_column,
                           sprintf("**{level}**\n(N = {style_number(n)}, {style_percent(p, digits = %d)}%%)",
                                   num_decimal_places),
                           "**{level}**\n(N = {style_number(n)})")

  table <- modify_header(
    table,
    label = "",
    all_stat_cols() ~ column_headers
  )

  # display percent if percent type is "column"
  total_col_stat <- ifelse(percent_type == "row", "{n}", categorical_stat)
  if (display_total_column) {
    table <- add_overall(
      table,
      last = TRUE,
      col_label = "**Source**\n(N = {style_number(n)})",
      statistic = list(all_categorical() ~ total_col_stat),
    )
  }

  # tab over the sublevels for pdf output, as it doesn't tab them over automatically
  table$table_body$label <- ifelse(output_format == "pdf" &
                                     (table$table_body$row_type == "level" | table$table_body$row_type == "missing"),
                                   paste0("\t", table$table_body$label),
                                   table$table_body$label)

  # for some reason values of 0 are outputting as 0(NA) automatically therefore,
  # the code below changes that to 0(0..) depending on the number formatting
  percent_symbol <- ifelse(display_percent_symbol, "%", "")
  stat_to_change <- sprintf("0 (NA%s)", percent_symbol)
  new_stat <- sprintf("0 (%s%s)",
                      style_number(0, digits = num_decimal_places, decimal.mark = decimal_mark),
                      percent_symbol)
  table$table_body$stat_1 <- ifelse(table$table_body$stat_1 == stat_to_change, new_stat, table$table_body$stat_1)
  table$table_body$stat_2 <- ifelse(table$table_body$stat_2 == stat_to_change, new_stat, table$table_body$stat_2)

  table <- bold_labels(table)
  table <- modify_footnote(table, all_stat_cols() ~ NA)

  if (output_to_csv){
    if (percent_type == "row"){
      out_file <- paste0(output_dir, "/linkage_rate_table.csv")
    } else {
      out_file <- paste0(output_dir, "/linked_data_representativeness_table.csv")
    }
    df <- as.data.frame(table)
    write.csv(df, out_file)
  }

  continuous_var_present <- ifelse("continuous" %in% table$table_body$var_type,
                            TRUE, FALSE)

  # transform gtsummary table into a flextable to ensure consistency in report output
  table <- as_flex_table(table)

  if (!display_unlinked_column){
    table <- delete_columns(table, 3)
  }

  # add extra footnote to end of provided footnotes
  if (continuous_var_present){
   default_footnote <- paste0("Data are presented as n (",
                             percent_type,
                             " %) or ",
                             ifelse(continuous_stat == "mean", "mean \u00B1 SD", "median (Q1, Q3)"),
                             "; where ",
                             ifelse(continuous_stat == "mean", "SD = standard deviation.", "Q1 = 25\u1d57\u02b0 percentile and Q3 = 75\u1d57\u02b0 percentile."))
  } else {
    default_footnote <- paste0("Data are presented as n (", percent_type," %)")
  }
  footnotes <- append(footnotes, default_footnote)

  # If we used a threshold, make a note of the columns that were removed
  if(length(removed_cols) > 0){
    removed_footnote <- paste0("The following variable(s) were excluded to suppress frequencies less than ", threshold, ": ", paste0(removed_cols, collapse = ", "))
    footnotes <- append(footnotes, removed_footnote)
  }

  table <- format_flextables_from_gtsummary(table,
                                            output_format,
                                            font_size,
                                            font_style,
                                            footnotes)


  return(table)
}
