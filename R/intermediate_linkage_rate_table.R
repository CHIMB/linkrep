#' Generate Intermediate Linkage Rate Table
#'
#' Generates a summary table of linkage rates stratified by characteristics.
#'
#' @param main_data_list A list of data frames containing the variables present in the left dataset
#' of the linkage.
#' @param main_data_algorithm_names A list of algorithm names for reporting the linkage
#'  rate and representativeness tables
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
#' @importFrom gtsummary theme_gtsummary_language tbl_summary as_flex_table modify_header modify_footnote add_overall bold_labels style_number all_categorical all_continuous all_stat_cols tbl_merge
#' @importFrom dplyr select mutate relocate across where
#' @importFrom tidyselect all_of
#' @importFrom Hmisc label
#' @importFrom rlang := !!
#' @importFrom utils write.csv
#'
#' @keywords internal
#' @noRd
#'
intermediate_linkage_rate_table <- function(main_data_list,
                                     main_data_algorithm_names,
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
  ### Handle Common Parameter Errors
  for(data in main_data_list){
    validate_common_parameters(data = data,
                               footnotes = footnotes,
                               output_format = output_format,
                               thousands_separator = thousands_separator,
                               decimal_mark = decimal_mark,
                               num_decimal_places = num_decimal_places,
                               display_percent_symbol = display_percent_symbol,
                               font_size = font_size,
                               font_style = font_style)
  }

  ### Modify the algorithm names
  for(i in 1:length(main_data_algorithm_names)){
    # Get the algorithm name
    algorithm_name <- main_data_algorithm_names[i]

    # Bold it
    algorithm_name <- paste0("**", algorithm_name, "**")

    # Place it back in
    main_data_algorithm_names[i] <- algorithm_name
  }

  ### Validation
  #----
  # Make sure the linked column and strata variable are good
  validate_string(column_var, "column_var")
  validate_string_vector(strata_vars, "strata_vars")

  # Validate the missing data indicators
  if (!is.null(missing_data_indicators)){
    validate_df(missing_data_indicators, "missing_data_indicators")
    validate_df_binary(missing_data_indicators, "missing_data_indicators")

    # Make the missing data indicators data frames
    missing_data_indicators <- as.data.frame(missing_data_indicators)
  }

  # Validate whether we want to display the total and unlinked columns
  validate_boolean(display_total_column, "display_total_column")
  validate_boolean(display_unlinked_column, "display_unlinked_column")

  # Verify what continuous stat we should be using
  validate_string(continuous_stat, "continuous_stat")
  if (continuous_stat != "median" & continuous_stat != "mean"){
    stop("Invalid argument: continuous_stat. Options: 'median' or 'mean'")
  }

  # Validate what percent type is to be used for this table
  validate_string(percent_type, "percent_type")
  if (percent_type != "row" & percent_type != "column"){
    stop("Invalid argument: percent_type. Options: 'row' or 'column'")
  }

  # Validate whether we should output to csv
  validate_boolean(output_to_csv, "output_to_csv")

  # Verify that an output directory exists
  if (!is.null(output_dir)){
    validate_string(output_dir, "output_dir")
    if (!dir.exists(output_dir)) {
      stop("Invalid argument: output_dir. output_dir must be a path to a directory")
    }
  }

  # For each dataset, verify it has the variable for determining whether a pair was linked, and
  # also verify it contains the necessary strata variables
  for(data in main_data_list){
    # Validate the "linked" variable
    validate_var_in_data(column_var, data, "column_var", "main_data")
    if (sum(is.na(data[[column_var]])) > 0 |
        sum(data[[column_var]] != 0 & data[[column_var]] != 1) > 0){
      stop("Invalid argument: column_var must be a binary or logical variable in 'main_data'")
    }

    # Validate the column variables
    invalid_strata_vars <- base::setdiff(strata_vars, names(data))
    if (length(invalid_strata_vars) > 0) {
      stop("Invalid argument: strata_vars. Not all variables provided in strata_vars are present in 'main_data'")
    }
    if (length(strata_vars) == 1 & is.null(missing_data_indicators)){
      if (strata_vars == column_var){
        stop("column_var and strata_vars cannot be the same")
      }
    }
  }

  # If the user wants to output a csv, make sure an output directory was provided
  if (output_to_csv & is.null(output_dir)){
    stop("output_dir must be provided when output_to_csv is TRUE")
  }
  #----

  # Change the gtsummary theme
  theme_gtsummary_language("en", decimal_mark, thousands_separator)

  # Before generating table summaries, remove small counts by noting the columns to avoid
  cols_to_drop   <- c()
  dropped_labels <- c()
  if(!is.null(threshold)){
    for(main_data in main_data_list){
      # data_subset will contain the necessary variables to generate the table
      data_subset <- select(main_data, all_of(strata_vars), all_of(column_var))
      # save labels to restore them after certain calculations remove them
      labels <- label(data_subset)

      # In all categorical variables, turn NA values into "Missing"
      data_subset <- mutate(data_subset, across(where(~ !is.numeric(.) & !is.integer(.)),
                                                ~ ifelse(is.na(.), "Missing",  if (is.factor(.)) as.character(.) else .)))

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

      # Generate the linkage table using just integer values
      table <- tbl_summary(
        data_subset,
        by = all_of(column_var),
        statistic = list(
          all_categorical() ~ "{n}",
          all_continuous() ~ "{mean}"
        )
      )

      # Get the table body
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
          stat <- table_body$stat_2[i]

          # Convert to numbers
          stat <- suppressWarnings(as.numeric(gsub(",", "", stat)))

          # Little function for checking if a number is whole or not
          is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

          # First, make sure the first and second value aren't NA, otherwise ignore the row
          if(!is.na(stat)){
            # Secondly, make sure that the first and second value are integers
            if(is.wholenumber(stat) == TRUE){
              # Lastly, check if the value are within the thresholds
              if((stat > 0 && stat <= threshold)){
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

      # After we finish going through the body, append the variables labels and remove duplicates
      cols_to_drop   <- append(cols_to_drop, variables_to_remove)
      cols_to_drop   <- unique(cols_to_drop)
      dropped_labels <- append(dropped_labels, labels_to_remove)
      dropped_labels <- unique(dropped_labels)
    }
  }

  # If the length of cols_to_drop is greater than 0, then we have at least one field to remove, so remove it
  if(length(cols_to_drop) > 0){
    strata_vars <- strata_vars[!strata_vars %in% cols_to_drop]
  }

  # Generate table summaries for EACH dataset
  all_summaries <- list()
  for(main_data in main_data_list){
    # data_subset will contain the necessary variables to generate the table
    data_subset <- select(main_data, all_of(strata_vars), all_of(column_var))
    # save labels to restore them after certain calculations remove them
    labels <- label(data_subset)

    # In all categorical variables, turn NA values into "Missing"
    data_subset <- mutate(data_subset, across(where(~ !is.numeric(.) & !is.integer(.)),
                                              ~ ifelse(is.na(.), "Missing",  if (is.factor(.)) as.character(.) else .)))

    # Re-order factors and restore labels
    for (i in names(data_subset)) {
      if ("Missing" %in% unique(data_subset[[i]])) {
        other_levels <- sort(setdiff(unique(data_subset[[i]]), "Missing"))
        data_subset[[i]] <- factor(data_subset[[i]], levels = c(other_levels, "Missing"))
      }
      if (labels[[i]] != "") {
        Hmisc::label(data_subset[[i]]) <- labels[[i]]
      }
    }

    # Determine the categorical and continuous stat
    categorical_stat <- ifelse(display_percent_symbol, "{n} ({p}%)", "{n} ({p})")
    continuous_stat_fmt <- ifelse(continuous_stat == "mean", "{mean} \u00B1 {sd}", "{median} ({p25}, {p75})")

    # identify factor levels used in column_var
    if ("logical" %in% unique(class(data_subset[[column_var]]))) {
      factor_levels <- c(TRUE, FALSE)
    } else {
      factor_levels <- c(1, 0)
    }

    # label the factor levels in column_var as "Linked" and "Unlinked"
    data_subset[[column_var]] <- factor(data_subset[[column_var]], levels = factor_levels, labels = c("Linked", "Unlinked"))

    # Generate the table summary for this dataset
    table <- tbl_summary(
      data_subset,
      by = all_of(column_var),
      statistic = list(
        all_categorical() ~ categorical_stat,
        all_continuous() ~ continuous_stat_fmt
      ),
      digits = list(
        all_categorical() ~ c(0, num_decimal_places),
        all_continuous() ~ num_decimal_places
      ),
      percent = percent_type,
      missing = "ifany",
      missing_text = "Missing"
    )

    # Get length of list and append
    len <- length(all_summaries)

    # Append the table
    all_summaries[[len+1]] <- table
  }

  # If we want to display a total column, then we will need to get that value first
  if(display_total_column){
    # data_subset will contain the necessary variables to generate the table
    data_subset <- select(main_data_list[[1]], all_of(strata_vars), all_of(column_var))
    # save labels to restore them after certain calculations remove them
    labels <- label(data_subset)

    # In all categorical variables, turn NA values into "Missing"
    data_subset <- mutate(data_subset, across(where(~ !is.numeric(.) & !is.integer(.)),
                                              ~ ifelse(is.na(.), "Missing",  if (is.factor(.)) as.character(.) else .)))

    # Re-order factors and restore labels
    for (i in names(data_subset)) {
      if ("Missing" %in% unique(data_subset[[i]])) {
        other_levels <- sort(setdiff(unique(data_subset[[i]]), "Missing"))
        data_subset[[i]] <- factor(data_subset[[i]], levels = c(other_levels, "Missing"))
      }
      if (labels[[i]] != "") {
        Hmisc::label(data_subset[[i]]) <- labels[[i]]
      }
    }

    # Determine the categorical and continuous stat
    categorical_stat <- ifelse(display_percent_symbol, "{n} ({p}%)", "{n} ({p})")
    continuous_stat_fmt <- ifelse(continuous_stat == "mean", "{mean} \u00B1 {sd}", "{median} ({p25}, {p75})")

    # identify factor levels used in column_var
    if ("logical" %in% unique(class(data_subset[[column_var]]))) {
      factor_levels <- c(TRUE, FALSE)
    } else {
      factor_levels <- c(1, 0)
    }

    # label the factor levels in column_var as "Linked" and "Unlinked"
    data_subset[[column_var]] <- factor(data_subset[[column_var]], levels = factor_levels, labels = c("Linked", "Unlinked"))

    # Generate the table summary for this dataset
    table <- tbl_summary(
      data_subset,
      by = all_of(column_var),
      statistic = list(
        all_categorical() ~ categorical_stat,
        all_continuous() ~ continuous_stat_fmt
      ),
      digits = list(
        all_categorical() ~ c(0, num_decimal_places),
        all_continuous() ~ num_decimal_places
      ),
      percent = percent_type,
      missing = "ifany",
      missing_text = "Missing"
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

    # Get length of list and append
    len <- length(all_summaries)

    # Append the table
    all_summaries[[len+1]] <- table

    # Combine summaries for all datasets dynamically
    combined_table <- tbl_merge(tbls = all_summaries, tab_spanner=c(main_data_algorithm_names, "**Source**"))
  }
  else{
    # Combine summaries for all datasets dynamically
    combined_table <- tbl_merge(tbls = all_summaries, tab_spanner=main_data_algorithm_names)
  }

  # Get column headers
  #column_headers <- ifelse(display_unlinked_column,
  #                         sprintf("**{level}**\n(N = {style_number(n)}, {style_percent(p, digits = %d)}%%)",
  #                                 num_decimal_places),
  #                         "**{level}**\n(N = {style_number(n)})")
  column_headers <- sprintf("**{level}**\n(N = {style_number(n)}, {style_percent(p, digits = %d)}%%)", num_decimal_places)

  # Apply column headers to the table
  table <- modify_header(
    combined_table,
    label = "",
    all_stat_cols() ~ column_headers
  )

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
  # Ensure `stat_1` and `stat_2` exist and contain data
  if (!is.null(table$table_body$stat_1) && length(table$table_body$stat_1) > 0) {
    table$table_body$stat_1 <- ifelse(table$table_body$stat_1 == stat_to_change, new_stat, table$table_body$stat_1)
  }
  if (!is.null(table$table_body$stat_2) && length(table$table_body$stat_2) > 0) {
    table$table_body$stat_2 <- ifelse(table$table_body$stat_2 == stat_to_change, new_stat, table$table_body$stat_2)
  }

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

  # Delete all the unlinked columns
  if (display_total_column){
    # Start from column 3 (this is the first "Unlinked" column)
    col_to_delete <- 3

    # Loop and delete until all "Unlinked" columns are gone
    while(col_to_delete <= length(main_data_algorithm_names) + 3){
      table <- delete_columns(table, col_to_delete)
      col_to_delete <- col_to_delete + 1
    }

    # Loop and delete until all "Overall" Columns are deleted
    col_to_delete <- 2 + length(main_data_algorithm_names)
    table <- delete_columns(table, col_to_delete)
  }
  else if (display_unlinked_column){
    # Start from column 3 (this is the first "Unlinked" column)
    col_to_delete <- 3

    # Loop and delete until all "Unlinked" columns are gone
    while(col_to_delete <= length(main_data_algorithm_names) + 2){
      table <- delete_columns(table, col_to_delete)
      col_to_delete <- col_to_delete + 1
    }
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

  # If the percent type is row %, then add extra text to footnote
  if(percent_type == "row"){
    default_footnote <- paste0(default_footnote, ', where the percentage indicates the row-wise linkage rate')
  }

  # Append footnote
  footnotes <- append(footnotes, default_footnote)

  # If we used a threshold, make a note of the columns that were removed
  if(length(dropped_labels) > 0){
    removed_footnote <- paste0("The following variable(s) were excluded to suppress frequencies less than ", threshold, ": ", paste0(dropped_labels, collapse = ", "))
    footnotes <- append(footnotes, removed_footnote)
  }

  table <- format_flextables_from_gtsummary(table,
                                            output_format,
                                            font_size,
                                            font_style,
                                            footnotes)


  return(table)
}
