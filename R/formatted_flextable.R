#' Create a Formatted Flextable
#'
#' @param data A data frame.
#' @param output_format A character string specifying the output format, must be
#'  one of "`pdf`" or "`docx`".
#' @param font_size A number specifying the font size for the table text.
#'  Default is \code{12}.
#' @param font_style A string specifying the font style. Must be present in
#'  \code{system_fonts()$name} or \code{system_fonts()$family}. See \code{\link[systemfonts]{system_fonts}}
#'  for more details.
#' @param footnotes A character vector of additional footnotes for
#' the table. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A string specifying the style of the
#'  thousands separator in all numeric values. Default is "\code{,}".
#' @param decimal_mark A string specifying the style of the decimal mark
#'  in all numeric values. Default is "\code{.}".
#' @param num_decimal_places A number specifying the number of digits to output
#'  after the decimal mark of all necessary numeric values. Default is \code{1}.
#' @param header_align A string specifying the alignment of the header,
#'  must be one of "\code{right}", "\code{left}" or "\code{center}".
#' @param body_align A string specifying the alignment of the body, must
#'  be one of "\code{right}", "\code{left}" or "\code{center}".
#'
#' @return A \code{flextable} object.
#'
#' @import flextable
#'
#' @keywords internal
#' @noRd
#'
formatted_flextable <- function(data,
                                output_format,
                                font_size,
                                font_style,
                                footnotes = NULL,
                                thousands_separator = ",",
                                decimal_mark = ".",
                                num_decimal_places = 1,
                                header_align = NULL,
                                body_align = NULL){

  # handle input errors
  validate_common_parameters(data = data,
                             output_format = output_format,
                             font_size = font_size,
                             font_style = font_style,
                             footnotes = footnotes,
                             thousands_separator = thousands_separator,
                             decimal_mark = decimal_mark,
                             num_decimal_places = num_decimal_places,
                             )

  if (!is.null(header_align)){
    validate_string(header_align, "header_align")
    if (header_align != "left" & header_align != "center" & header_align != "right"){
      stop("Invalid argument: header_align. Must be one of 'left', 'center', 'right'")
    }
  }

  if (!is.null(body_align)){
    validate_string(body_align, "body_align")
    if (body_align != "left" & body_align != "center" & body_align != "right"){
      stop("Invalid argument: body_align. Must be one of 'left', 'center', 'right'")
    }
  }

  table <- flextable(data)

  #----
  # set_flextable_labels
  #
  # since flextable doesn't use variable labels automatically, this function
  # renames all the header labels in the flextable to match the labels given in 'data'.
  # if there are no labels in 'data', it leaves the name of the variable as is.
  # return: the flextable with updated header labels
  #----
  set_flextable_labels <- function(data, ft){

    validate_df(data, "data")
    validate_flextable(ft, "ft")

    labels <- lapply(data, function(x) attr(x, "label"))
    for (i in 1:length(labels)){
      if (is.null(labels[[i]])){
        labels[[i]] <- names(data)[i]
      }
    }
    ft <- do.call(set_header_labels, c(list(ft), labels))
    return(ft)
  }

  table <- set_flextable_labels(data, table)

  if(!is.null(footnotes)) {
    table <- add_footer_lines(table, values = footnotes)
  }

  # format the numbers in the table
  table <- colformat_int(table, big.mark = thousands_separator)
  table <- colformat_double(table,
                            big.mark = thousands_separator,
                            decimal.mark = decimal_mark,
                            digits = num_decimal_places)


  if (!is.null(header_align)){
    table <- flextable::align(table, align = header_align, part = "header")
  }

  if (!is.null(body_align)){
    table <- flextable::align(table, align = body_align, part = "body")
  }

  table <- fontsize(table, size = font_size, part = "all")
  table <- font(table, fontname = font_style, part = "all")

  if (output_format == "pdf"){
    table <- set_table_width(table)
  }

  return(table)
}
