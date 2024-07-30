#' Generate Linkage Rate Table
#'
#' Generates a summary table of linkage rates stratified by characteristics.
#'
#' @param main_data
#' @param output_format
#' @param column_var
#' @param strata_vars
#' @param main_data_missing_field_indicators_data
#' @param display_total_column
#' @param display_mean_not_median_stats
#' @param display_alphabetically
#' @param font_size
#' @param font_style
#' @param footnotes
#' @param thousands_separator
#' @param decimal_mark
#' @param num_decimal_places
#' @param display_percent_symbol
#' @param output_to_csv
#' @param output_dir
#'
#' @return A `flextable` that was originally a `gtsummary`.
#'
#' @importFrom gtsummary theme_gtsummary_language tbl_summary as_flex_table modify_header modify_footnote add_overall bold_labels
#' @importFrom dplyr select mutate relocate
#' @importFrom tidyselect all_of
#' @importFrom Hmisc label
#'
#' @examples ###########NOT DONE!!!!!!!!
linkage_rate_table <- function(main_data,
                               output_format,
                               column_var,
                               strata_vars,
                               main_data_missing_field_indicators_data = NULL, # need to do checks! especially with it maybe being NULL
                               display_total_column = TRUE,
                               display_mean_not_median_stats = FALSE,
                               display_alphabetically = FALSE, # need to implement!!
                               font_size = 12,
                               font_style = "Times New Roman",
                               footnotes = NULL,
                               thousands_separator = ",",
                               decimal_mark = ".",
                               num_decimal_places = 1,
                               display_percent_symbol = FALSE,
                               output_to_csv = FALSE,
                               output_dir = NULL
)

{
  # handle input errors
  validate_parameters(data = main_data,
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

  if (!is.null(main_data_missing_field_indicators_data)){
    validate_df(main_data_missing_field_indicators_data, "main_data_missing_field_indicators_data")
    validate_df_binary(main_data_missing_field_indicators_data, "main_data_missing_field_indicators_data")

    # need these to be dataframes to use dplyr manipulation below
    main_data <- as.data.frame(main_data)
    main_data_missing_field_indicators_data <- as.data.frame(main_data_missing_field_indicators_data)
  }

  validate_boolean(display_total_column, "display_total_column")
  validate_boolean(display_mean_not_median_stats, "display_mean_not_median_stats")
  validate_boolean(display_alphabetically, "display_alphabetically")
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
    stop("Invalid argument: column_var must be binary or logical")
  }

  invalid_strata_vars <- base::setdiff(strata_vars, names(main_data))
  if (length(invalid_strata_vars) > 0) {
    stop("Invalid argument: strata_vars. Not all variables provided in strata_vars are present in 'data'")
  }
  if (length(strata_vars) == 1 & is.null(main_data_missing_field_indicators_data)){
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

  if (!is.null(main_data_missing_field_indicators_data)){
    # missing indicators original labels
    missing_labels <- label(main_data_missing_field_indicators_data)

    # Match the columns in the two datasets and label the matched missing indicators "Missing"
    i <- 1
    while(i <= ncol(data_subset) & ncol(main_data_missing_field_indicators_data) > 0){
      data_subset_col_name <- names(data_subset)[i]
      # naming standard for missing field indicators
      missing_col_name <- paste0(data_subset_col_name, "_missing")

      if (missing_col_name %in% names(main_data_missing_field_indicators_data)) {
        data_subset <- mutate(data_subset, !!missing_col_name := main_data_missing_field_indicators_data[[missing_col_name]])
        # need variable to be right after the 'main_data' variable to be able to make it a sublevel of it in the table
        data_subset <- relocate(data_subset, !!missing_col_name, .after = !!data_subset_col_name)
        label(data_subset[[missing_col_name]]) <- "Missing"
        main_data_missing_field_indicators_data[[missing_col_name]] <- NULL
        i <- i + 2
      } else {
        col_label <- label(data_subset[[i]])
        if (col_label %in% missing_labels) {
          missing_index <- which(missing_labels == col_label)
          missing_col_name <- names(missing_labels)[missing_index]
          data_subset <- mutate(data_subset, !!missing_col_name := main_data_missing_field_indicators_data[[missing_index]])
          # need variable to be right after the 'main_data' variable to be able to make it a sublevel of it in the table
          data_subset <- relocate(data_subset, !!missing_col_name, .after = !!data_subset_col_name)
          label(data_subset[[missing_col_name]]) <- "Missing"
          main_data_missing_field_indicators_data[[missing_col_name]] <- NULL
          i <- i + 1
        }
        i <- i + 1
      }
    }

    # label remaining missing indicators with "Missing " in front of the label or variable name so it stands on its own in the table
    if (ncol(main_data_missing_field_indicators_data) > 0) {
      for (i in seq_along(main_data_missing_field_indicators_data)){
        col_label <- label(main_data_missing_field_indicators_data[,i])
        if (col_label == ""){
          label(main_data_missing_field_indicators_data[,i]) <- paste("Missing", names(main_data_missing_field_indicators_data)[i])
        } else {
          label(main_data_missing_field_indicators_data[,i]) <- paste("Missing", col_label)
        }
      }
    }

    # combine the two datasets
    data_subset <- cbind(data_subset, main_data_missing_field_indicators_data)
  }

  categorical_stat <- ifelse(display_percent_symbol, "{n} ({p}%)", "{n} ({p})")
  continuous_stat <- ifelse(display_mean_not_median_stats,
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


  # generate linkage rate table
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
    percent = "row",
    missing = "no"
  )

  table <- modify_header(
    table,
    label = "",
    update = all_stat_cols() ~ sprintf("**{level}**\n(N = {style_number(n)}, {style_percent(p, digits = %d)}%%)",
                                       num_decimal_places)
  )

  if (display_total_column) {
    table <- add_overall(
      table,
      last = TRUE,
      col_label = "**Total**\n(N = {style_number(n)})", #changed <br> to \n
      statistic = list(all_categorical() ~ "{N}",
                       all_dichotomous() ~ "{N}",
                       all_continuous() ~ continuous_stat),
    )
  }

  # make the missing values associated with another variable a sub level of that variable
  table$table_body$row_type <- ifelse(table$table_body$var_label == "Missing", "level", table$table_body$row_type)

  # tab over the sublevels for pdf output, as it doesn't tab them over automatically
  table$table_body$label <- ifelse(output_format == "pdf" & table$table_body$row_type == "level",
                                   paste0("\t", table$table_body$label),
                                   table$table_body$label)

  # change odd 0 (NA) output to 0 (0...)
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
    out_file <- paste0(output_dir, "/linkage_rate_table.csv")
    df <- as.data.frame(table)
    write.csv(df, out_file)
  }

  # transform gtsummary table into a flextable
  table <- as_flex_table(table)

  if (is.null(footnotes)) {
    footnotes <- 'Data are presented as n (%), mean \u00B1 SD, or median (Q1, Q3); where SD = standard deviation, Q1 = 25\u1d57\u02b0 percentile and Q3 = 75\u1d57\u02b0 percentile.'
  } else {
    footnotes <- paste(footnotes, collapse = "\n")
    footnotes <- paste0(
      footnotes,
      "\n",
      'Data are presented as n (%), mean \u00B1 SD, or median (Q1, Q3); where SD = standard deviation, Q1 = 25\u1d57\u02b0 percentile and Q3 = 75\u1d57\u02b0 percentile.'
    )
  }

  table <- format_flextables_from_gtsummary(table,
                                            output_format,
                                            font_size,
                                            font_style,
                                            footnotes)

  return(table)
}
