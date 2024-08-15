#' Generate Radar Chart of Performance Measures
#'
#' @param data A single row data frame with values ranging from 0 to 100.
#'
#' @return A radar chart.
#'
#' @importFrom fmsb radarchartcirc
#'
performance_measures_plot <- function(data){

  PLOT_COLOUR <- "grey65"
  CIRCLES_COLOUR <- "grey20"
  AXIS_LABELS_COLOUR <- "grey20"
  MIN_DATA_VALUE <- 0
  MAX_DATA_VALUE <- 100
  MAX_LABEL_LENGTH <- 12

  validate_df(data, "data")

  if (nrow(data) != 1){
    warning("performance measures is more than one row therefore radar chart will not be output")
  } else if (ncol(data) < 3){
    warning("must be more than 2 variables present in performance measures to create radar chart therefore, radar chart will not be output")
  } else if (!all(sapply(data, class) %in% c("numeric","integer"))){
    warning("performance measures are not all numeric therefore radar chart will not be output")
  } else if (!(all(data[1,] >= 0 & data[1,] <= 100))){
    warning("performance measures are not between 0 and 100 therefore radar chart will not output")
  } else {
    # since radarchartcirc doesn't use variable labels automatically, get them and set them manually in the function call
    labels <- lapply(data, function(x) attr(x, "label"))
    for (i in 1:length(labels)){
      if (is.null(labels[[i]])){
        labels[[i]] <- names(data)[i]
      }
      if (nchar(labels[[i]]) > MAX_LABEL_LENGTH & nchar(labels[[i]]) != MAX_LABEL_LENGTH + 1){
        # truncate the label
        labels[[i]] <- paste0(substr(labels[[i]], 1, 10), "...")
      }
    }
    labels <- unlist(labels)

    data <- as.data.frame(data)
    data <- rbind(max = rep(MAX_DATA_VALUE, ncol(data)),
                  min = rep(MIN_DATA_VALUE, ncol(data)),
                  data)

    radarchartcirc(data,
                   axistype = 1,
                   seg = 5,
                   pcol = PLOT_COLOUR,
                   pfcol = alpha(PLOT_COLOUR, 0.5),
                   plwd = 2,
                   cglcol = CIRCLES_COLOUR,
                   cglty = 1,
                   axislabcol = AXIS_LABELS_COLOUR,
                   caxislabels = seq(0, 100, 20),
                   cglwd = 0.8,
                   vlcex = 0.8,
                   vlabels = labels)
  }
}



