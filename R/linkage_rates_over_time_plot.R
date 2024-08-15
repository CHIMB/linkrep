#' Generate Barplot of Linkage Rates by Acquisition Dates
#'
#' Generates a barplot of linkage rates by acquisition dates with the x-axis
#' laid out depending on the number of unique years present in \code{data}.
#'
#' @param data A data frame containing the variables present in the left dataset
#' of the linkage.
#' @param link_indicator_var A string of the name of a logical or binary
#'  variable present in \code{data} that indicates whether a record linked or not.
#' @param acquisition_year_var A string of the name of the numeric variable in
#'  \code{data} that represents the acquisition year.
#' @param acquisition_month_var A string of the name of the numeric variable in
#'  \code{data} that represents the acquisition month.
#'
#' @details
#' The x-axis will either be displayed in monthly, quarterly, biyearly or years
#' depending on the range of the acquisition dates provided.
#'
#' In order for the plot to be output, there needs to be enough range in the acquisition
#' dates to produce at least five bars in the barplot.
#'
#' @return A barplot of linkage rates by acquisition date.
#'
#' @import ggplot2
#' @importFrom dplyr mutate summarise group_by ungroup select
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @importFrom lubridate make_date quarter semester
#'
linkage_rates_over_time_plot <- function(data,
                                         link_indicator_var,
                                         acquisition_year_var,
                                         acquisition_month_var)
{
  DISPLAY_MONTHLY <- 2
  DISPLAY_QUARTERLY <- 6
  DISPLAY_BIYEARLY <- 10
  MIN_NUM_BARS <- 5

  plot <- NULL

  validate_df(data, "data")

  validate_string(link_indicator_var, "link_indicator_var")
  if (sum(is.na(data[[link_indicator_var]])) > 0 |
      sum(data[[link_indicator_var]] != 0 & data[[link_indicator_var]] != 1) > 0){
    stop("Invalid argument: link_indicator_var must be binary or logical")
  }
  validate_var_in_data(link_indicator_var, data, "link_indicator_var", "data")

  validate_string(acquisition_year_var, "acquisition_year_var")
  validate_var_in_data(acquisition_year_var, data, "acquisition_year_var", "data")
  validate_numeric_vector(data[[acquisition_year_var]], "acquisition_year_var")

  validate_string(acquisition_month_var, "acquisition_month_var")
  validate_var_in_data(acquisition_month_var, data, "acquisition_month_var", "data")
  validate_numeric_vector(data[[acquisition_month_var]], "acquisition_month_var")
  if (!all(data[[acquisition_month_var]] %in% c(1:12, NA))){
    stop("acquisition_month_var contains invalid values. Must be either NA or numbers from 1 to 12.")
  }

  #----
  # get_year_x_breaks
  #
  # This function calculates where the year labels should go under the quarter
  # or half labels along the x-axis depending on the number of bars outputting
  # for that year.
  # return: the x locations of the year labels
  #----
  get_year_x_breaks <- function(data, acquisition_year_var){
    x_breaks <- c()
    min_year = min(data[[acquisition_year_var]])
    curr_location <- 1
    for (i in 1:length(unique(data[[acquisition_year_var]]))){
      num_bins_curr_year <- sum(data[[acquisition_year_var]] == min_year+i-1)
      if (num_bins_curr_year == 1){
        x_breaks[i] <- curr_location
        curr_location <- curr_location + 1
      } else if (num_bins_curr_year == 2){
        x_breaks[i] <- curr_location + 0.5
        curr_location <- curr_location + 2
      } else if (num_bins_curr_year == 3) {
        x_breaks[i] <- curr_location + 1
        curr_location <- curr_location + 3
      } else {
        x_breaks[i] <- curr_location + 1.5
        curr_location <- curr_location + 4
      }
    }
    return(x_breaks)
  }

  num_years <- length(unique(data[[acquisition_year_var]]))
  data <- mutate(data, date = make_date(.data[[acquisition_year_var]], .data[[acquisition_month_var]], 1))

  if (num_years <= DISPLAY_MONTHLY){
    # display monthly
    data <- summarise(group_by(data,
                               .data[[acquisition_year_var]],
                               .data[[acquisition_month_var]],
                               date),
                      linkage_rate = mean(.data[[link_indicator_var]]) * 100) |> ungroup()
    data <- drop_na(data)

    num_bars <- nrow(data)
    if (num_bars >= MIN_NUM_BARS){
      plot <- ggplot(data, aes(x = date, y = linkage_rate)) +
        geom_col() +
        labs(x = "Acquisition Date", y = "Linkage Rate (%)") +
        scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0, max(data$linkage_rate), 10), expand = expansion(mult = c(0, .03))) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

  } else if (num_years <= DISPLAY_QUARTERLY){
    # display quarterly
    data <- mutate(data, quarter = quarter(date))
    data <- mutate(data, quarter = paste0("Q", quarter))
    data <- summarise(group_by(data,
                               .data[[acquisition_year_var]],
                               quarter),
                      linkage_rate = mean(.data[[link_indicator_var]]) * 100) |> ungroup()
    data <- drop_na(data)
    x_breaks <- get_year_x_breaks(data, acquisition_year_var)

    num_bars <- nrow(data)
    if (num_bars >= MIN_NUM_BARS){
      plot <- ggplot(data, aes(x = interaction(quarter, .data[[acquisition_year_var]]), y = linkage_rate)) +
        geom_col() +
        labs(x = "Acquisition Date (Q = Quarter)", y = "Linkage Rate (%)") +
        coord_cartesian(ylim = c(0, max(data$linkage_rate) + 2), expand = FALSE, clip = "off") +
        annotate(geom = "text", x = seq_len(nrow(data)), y = -3, label = data$quarter, size = 3) +
        annotate(geom = "text", x = x_breaks, y = -7.5, label = unique(data[[acquisition_year_var]]), size = 3.5) +
        theme(
          plot.margin = unit(c(1, 1, 3, 1), "lines"),
          axis.title.x = element_text(vjust = -9.5),
          axis.text.x = element_blank()
        ) +
        scale_y_continuous(breaks = seq(0, max(data$linkage_rate), 10))
    }

  } else if (num_years <= DISPLAY_BIYEARLY){
    # display bi-yealry
    data <- mutate(data, half = semester(date, with_year = FALSE))
    data <- mutate(data, half = paste0("H", half))
    data <- summarise(group_by(data, .data[[acquisition_year_var]], half),
                      linkage_rate = mean(.data[[link_indicator_var]]) * 100) |> ungroup()
    data <- drop_na(data)
    x_breaks <- get_year_x_breaks(data, acquisition_year_var)

    num_bars <- nrow(data)
    if (num_bars >= MIN_NUM_BARS){
      plot <- ggplot(data, aes(x = interaction(half, .data[[acquisition_year_var]]), y = linkage_rate)) +
        geom_col() +
        labs(x = "Acquisition Date (H = Half)", y = "Linkage Rate (%)") +
        coord_cartesian(ylim = c(0, max(data$linkage_rate) + 2), expand = FALSE, clip = "off") +
        annotate(geom = "text", x = seq_len(nrow(data)), y = -3, label = data$half, size = 3) +
        annotate(geom = "text", x = x_breaks, y = -7.5, label = unique(data[[acquisition_year_var]]), size = 3.5) +
        theme(
          plot.margin = unit(c(1, 1, 3, 1), "lines"),
          axis.title.x = element_text(vjust = -9.5),
          axis.text.x = element_blank(),
        ) +
        scale_y_continuous(breaks = seq(0, max(data$linkage_rate), 10))
    }

  } else {
    # display yearly
    data <- select(data, all_of(acquisition_year_var), all_of(link_indicator_var))
    data <- summarise(group_by(data, .data[[acquisition_year_var]]),
                      linkage_rate = mean(.data[[link_indicator_var]]) * 100) |> ungroup()
    data <- drop_na(data)

    num_bars <- nrow(data)
    if (num_bars >= MIN_NUM_BARS){
      plot <- ggplot(data, aes(x = .data[[acquisition_year_var]], y = linkage_rate)) +
        geom_col() +
        labs(x = "Acquisition Year", y = "Linkage Rate (%)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = seq(min(data[[acquisition_year_var]]), max(data[[acquisition_year_var]]), 2), expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0, max(data$linkage_rate), 10), expand = expansion(mult = c(0, .03)))
    }
  }

  return(plot)
}

