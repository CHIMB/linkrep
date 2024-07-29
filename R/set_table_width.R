#' Set the Width of a Flextable
#'
#' Sets the width of the table based on its desired width (\code{\link{dim_pretty}})
#' and the max width the table can be based on the page size. The column widths
#' are adjusted as needed to fit the page.
#'
#' @param flextable A `flextable` object.
#'
#' @return The `flextable` adjusted to the desired width
#' @import flextable
#'
set_table_width <- function(flextable){
  PORTRAIT_WIDTH <- 6.5
  LANDSCAPE_WIDTH <- 9
  MIN_COL_WIDTH <- 0.8

  validate_flextable(flextable)

  desired_dims <- dim_pretty(flextable)
  curr_width <- sum(desired_dims$widths)
  max_width <- PORTRAIT_WIDTH

  if (curr_width >= PORTRAIT_WIDTH) {
    max_width <- LANDSCAPE_WIDTH
  }

  if (max_width == PORTRAIT_WIDTH) {
    flextable <- autofit(flextable)
  } else {
    if (curr_width < LANDSCAPE_WIDTH) {
      flextable <- autofit(flextable)
    } else {
      num_cols <- ncol_keys(flextable)
      widths <- desired_dims$widths
      sorted_widths <- sort(widths) #widths sorted
      column_order <- order(widths) # column numbers of sorted widths

      # 0.5 leaves some wiggle room as the flextable width calculation is not completely accurate
      excess_width <- curr_width - LANDSCAPE_WIDTH + 0.5 # will be > 0
      col_reduction <- excess_width / num_cols

      for (i in 1:num_cols){
        curr_width <- sorted_widths[i]
        new_width <- curr_width - col_reduction
        col_num <- column_order[i]

        if (new_width < MIN_COL_WIDTH) {
          if (MIN_COL_WIDTH < curr_width){
            new_width <- MIN_COL_WIDTH
          } else {
            new_width <- curr_width
          }
          reduction <- sorted_widths[i] - new_width
          excess_width <- excess_width - reduction
          col_reduction <- excess_width / (num_cols-i)
        }
        flextable <- width(flextable, col_num, new_width)
      }
    }

  }
  return(flextable)
}
