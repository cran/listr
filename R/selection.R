#' Select elements from a list using 'tidyselect'.
#'
#' @param in_list The list to select from.
#' @param ... The selection specification.
#' @param enumerate If no names are present set enumeration as names.
#'
#' @return A 'tidyselect' selection
#' @noRd
#' @keywords internal
selection <- function(in_list, ..., enumerate = FALSE) {
    if (is.null(names(in_list)) & enumerate) {
        names(in_list) <- as.character(seq_len(length(in_list)))
    }

    expr <- rlang::expr(c(...))
    selection <- tidyselect::eval_select(expr, in_list)
    selection
}
