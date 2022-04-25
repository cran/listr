#' Select parts of a list.
#'
#' @param in_list The list to select from.
#' @param ... The selection, can both be names and numeric positions.
#'
#' @details
#' `list_select()` lets you select parts of a list either by position or
#' by name.
#' Names can be supplied as bare variable names and do not need to be
#' supplied as strings or otherwise be quoted.
#'
#' Elements are returned in the order they are given, this is useful if you
#' want to reorder elements in a list.
#' You can also rename while selecting, writing your selection like
#' `new_name = old_name`.
#'
#' @return A list of the selected elements.
#' @export
#' @examples
#' my_list <- list(a = c(1, 2), b = c(3, 4), c(5, 6))
#' list_select(my_list, a, 3)
list_select <- function(in_list, ...) {
    unnamed_input <- ifelse(is.null(names(in_list)), TRUE, FALSE)
    selection <- selection(in_list, c(...), enumerate = unnamed_input)

    sel_list <- in_list[selection]
    names(sel_list) <- names(selection)

    if (unnamed_input) {
        names(sel_list)[names(sel_list) == as.character(selection)] <- ""
    }

    sel_list
}
