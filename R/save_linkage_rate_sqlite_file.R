#' Save Information on Linkage in SQLite File
#'
#' @param output_dir A path to a directory. The SQLite file will be saved here.
#' @param report_generation_date The date (formatted) the report was generated.
#' @param data_linker String indicating who performed the linkage.
#' @param left_dataset_name String indicating the name of the left dataset.
#' @param right_dataset_name String indicating the name of the right dataset.
#' @param overall_linkage_rate A formatted number indicating the overall linkage rate.
#' @param data_time_period The acquisition dates of the left dataset.
#' @param project_id The project ID.
#'
#' @return An SQLite file will be saved in \code{output_dir}
#'
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#'
#' @keywords internal
#' @noRd
#'
save_linkage_rate_sqlite_file <- function(output_dir,
                                          report_generation_date,
                                          data_linker,
                                          left_dataset_name,
                                          right_dataset_name,
                                          overall_linkage_rate,
                                          data_time_period = NULL,
                                          project_id = NULL){

  # parameter checks

  validate_string(output_dir, "output_dir")
  if (!dir.exists(output_dir)) {
    stop("Invalid argument: output_dir. output_dir must be a path to a directory")
  }
  validate_string(report_generation_date, "report_generation_date")
  validate_string(data_linker, "data_linker")
  validate_string(left_dataset_name, "left_dataset_name")
  validate_string(right_dataset_name, "right_dataset_name")
  validate_string(overall_linkage_rate, "overall_linkage_rate")
  if (!is.null(data_time_period)){
    validate_string(data_time_period, "data_time_period")
  } else {
    data_time_period <- NA
  }
  if (!is.null(project_id)){
    if (length(project_id) != 1){
      stop("Invalid argument: project_id. project_id must be a single input.")
    }
  } else {
    project_id <- NA
  }

  sqlite_file <- file.path(output_dir, "linkage_rate.sqlite")
  con <- dbConnect(SQLite(), sqlite_file)

  on.exit(dbDisconnect(con))

  data <- data.frame(
    report_generation_date = report_generation_date,
    report_generation_year = as.numeric(format(Sys.Date(), "%Y")),
    data_linker = data_linker,
    left_dataset_name = left_dataset_name,
    right_dataset_name = right_dataset_name,
    overall_linkage_rate = overall_linkage_rate,
    acquisition_dates_left_dataset = data_time_period,
    project_id = project_id
  )

  dbWriteTable(con, 'linkage_rate', data, overwrite = TRUE)
}

