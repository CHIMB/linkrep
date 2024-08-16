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
#' @param display_mean_not_median_stats A logical indicating whether
#'  to display the statistics for continuous variables in the table
#'  as either mean \eqn{\pm} standard deviation or median (Q1, Q3), where Q1 is
#'  the 25\eqn{^{th}} percentile, and Q3 is the 75\eqn{^{th}} percentile. Default
#'  is \code{FALSE}.
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
#' @importFrom dplyr select mutate relocate
#' @importFrom tidyselect all_of
#' @importFrom Hmisc label
#' @importFrom rlang := !!
#' @importFrom utils write.csv
#'
linkage_rate_table <- function(main_data,
                               output_format,
                               column_var,
                               strata_vars,
                               missing_data_indicators = NULL,
                               display_total_column = TRUE,
                               display_mean_not_median_stats = FALSE,
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
  validate_boolean(display_mean_not_median_stats, "display_mean_not_median_stats")
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

  #----
  # We decided to have two separate data frames passed to the linkage rate table
  # to make it easier to identify missing variables to be used in the missingness
  # table. If one data frame were passed to the main function then we would need
  # to ensure all missing values were indicated with NA and then perform computations
  # in the missignness table function to extract the missing values and make a whole
  # new data frame to generate that table. We felt by using this strategy, we eliminate
  # the need for making a new data frame and leave it up to the user to ensure
  # they provide a missingness data frame if they want missing values output.
  #
  # Logic for below:
  # 1) Loop through main_data, each time trying to identify if there is a corresponding
  # variable in missing_data_indicators
  # - a corresponding variable is either one with the same variable name suffixed by "_missing"
  #   or the same label as the variable in main_data
  # 2) If a corresponding variable is found, place the variable from missing_data_indicators
  # next to the one in main_data (this is needed for later on). Remove the
  # missing_data_indicators variable from missing_data_indicators. Label that variable "Missing".
  # 3) Once the loop is finished:
  # - If there are remaining variables in missing_data_indicators, append them to the
  #   end of main_data and prefix either their label, or if they don't have one their
  #   variable name, by "Missing "
  # - All variables provided in missing_data_indicators will be output in the table
  #----
  if (!is.null(missing_data_indicators)){
    # Match the columns in the two datasets and label the matched missing indicators "Missing"
    i <- 1
    while(i <= ncol(data_subset) & ncol(missing_data_indicators) > 0){
      # missing indicators labels
      missing_labels <- label(missing_data_indicators)

      data_subset_col_name <- names(data_subset)[i]
      # naming standard for missing field indicators
      missing_col_name <- paste0(data_subset_col_name, "_missing")

      # two options:
      # 1. The name of the variable in main_data_missing matches the name of the variable in main_data suffixed by '_missing'
      # 2. The label of the variable in main_data_missing matches the label of the variable in main_data
      if (missing_col_name %in% names(missing_data_indicators)) {
        data_subset <- mutate(data_subset, !!missing_col_name := missing_data_indicators[[missing_col_name]])
        # need variable to be right after the 'main_data' variable to be able to make it a sublevel of it in the table
        data_subset <- relocate(data_subset, !!missing_col_name, .after = !!data_subset_col_name)
        Hmisc::label(data_subset[[missing_col_name]]) <- "Missing"
        missing_data_indicators[[missing_col_name]] <- NULL
        i <- i + 2
      } else {
        col_label <- label(data_subset[[i]])
        if ((col_label != "") & (col_label %in% missing_labels)) {
          missing_index <- which(missing_labels == col_label)
          missing_col_name <- names(missing_labels)[missing_index]
          data_subset <- mutate(data_subset, !!missing_col_name := missing_data_indicators[[missing_index]])
          # need variable to be right after the 'main_data' variable to be able to make it a sublevel of it in the table
          data_subset <- relocate(data_subset, !!missing_col_name, .after = !!data_subset_col_name)
          Hmisc::label(data_subset[[missing_col_name]]) <- "Missing"
          missing_data_indicators[[missing_col_name]] <- NULL
          i <- i + 1
        }
        i <- i + 1
      }
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
                       all_continuous() ~ continuous_stat),
    )
  }

  #----
  # Above we moved the missing_data_indicator variables associated with a main_data
  # variable right next to its corresponding variable in main_data. This was so
  # we could easily locate the "Missing" variable and make it a 'level' of its
  # main variable. This way, "Missing" will show up as a subgroup of that variable
  # in the table and won't be bold alongside the main variables.
  #----

  # make the missing values associated with another variable a sub level of that variable
  table$table_body$row_type <- ifelse(table$table_body$var_label == "Missing", "level", table$table_body$row_type)

  # tab over the sublevels for pdf output, as it doesn't tab them over automatically
  table$table_body$label <- ifelse(output_format == "pdf" & table$table_body$row_type == "level",
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
    out_file <- paste0(output_dir, "/linkage_rate_table.csv")
    df <- as.data.frame(table)
    write.csv(df, out_file)
  }

  # transform gtsummary table into a flextable to ensure consistency in report output
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
