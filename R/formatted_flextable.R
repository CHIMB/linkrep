#' Create a Formatted Flextable
#'
#' @param data A data frame.
#' @param output_format A character string specifying the output format, must be one of "`pdf`" or "`docx`".
#' @param font_size A numeric specifying the font size for the table text.
#' @param font_style A character string specifying the font. Must be present in \code{system_fonts()$name} or \code{system_fonts()$family} in the package \code{\link{system_fonts}}
#' @param footnotes A character vector of additional footnotes. Each element in the vector will be displayed on a new line.
#' @param thousands_separator A character string specifying the thousands separator.
#' @param decimal_mark A character string specifying the decimal mark.
#' @param num_decimal_places A numeric specifying the number of digits to output after the decimal mark.
#' @param header_align A character string specifying the alignment of the header, must be one of "`right`", "`left`" or "`center`".
#' @param body_align A character string specifying the alignment of the body, must be one of "`right`", "`left`" or "`center`".
#'
#' @return A `flextable`
#'
#' @import flextable
#'
formatted_flextable <- function(data,
                                output_format,
                                font_size,
                                font_style,
                                footnotes = NULL,
                                thousands_separator = NULL,
                                decimal_mark = NULL,
                                num_decimal_places = NULL,
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
    table <- add_footer_lines(table, values = paste(footnotes, collapse = "\n"))
  }

  # format the numbers in the table
  if (!is.null(thousands_separator)){
    table <- colformat_double(table,
                              big.mark = thousands_separator)
    table <- colformat_int(table,
                           big.mark = thousands_separator)
  }

  if (!is.null(decimal_mark)){
    table <- colformat_double(table, decimal.mark = decimal_mark)
  }

  if (!is.null(num_decimal_places)){
    table <- colformat_double(table, digits = num_decimal_places)
  }

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
