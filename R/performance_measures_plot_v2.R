#' Generate Radar Chart of Performance Measures
#'
#' @param data A single row data frame with values ranging from 0 to 100.
#'
#' @return A radar chart.
#'
#' @importFrom fmsb radarchartcirc
#'
#' @keywords internal
#' @noRd
#'
performance_measures_plot_v2 <- function(data, col_index){

  PLOT_COLOUR <- "grey65"
  PLOT_COLOURS <- c("red", "orange", "yellow", "green", "lightblue", "blue", "violet", "pink", "grey80", "grey70", "grey60", "grey50")
  CIRCLES_COLOUR <- "grey20"
  AXIS_LABELS_COLOUR <- "grey20"
  MIN_DATA_VALUE <- 0
  MAX_DATA_VALUE <- 100
  MAX_LABEL_LENGTH <- 12

  validate_df(data, "data")

  # Grab the algorithm names
  algorithm_names <- data$algorithm_name
  data <- subset(data, select = -algorithm_name)

  if (ncol(data) < 4){
    warning("must be more than 2 variables present in performance measures to create radar chart therefore, radar chart will not be output")
  } else if (!all(sapply(data, class) %in% c("numeric","integer", "labelled"))){
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
                   pcol = alpha(PLOT_COLOURS[col_index], 0.8),
                   pfcol = alpha(PLOT_COLOURS[col_index], 0.5),
                   plty = 1,
                   plwd = 2,
                   cglcol = CIRCLES_COLOUR,
                   cglty = 1,
                   axislabcol = AXIS_LABELS_COLOUR,
                   caxislabels = seq(0, 100, 20),
                   cglwd = 0.8,
                   vlcex = 1.1,
                   vlabels = labels
                   )

    # legend(x = "bottom", legend = algorithm_names, horiz = T,
    #       bty = "n", pch = 20, col = PLOT_COLOURS, text.col = "black",
    #       cex = 1, pt.cex = 1.5)
  }
}



