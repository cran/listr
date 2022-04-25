#' Rename elements of a named list.
#'
#' @param in_list The list to rename elements in.
#' @param ... The renaming definitions.
#'
#' @details
#' `list_rename()` changes the name of elements in a named list. The definitions
#' for renaming are given in `...` in the style `new_name = old_name`.
#' You can specify as many renaming definitions as you like as long as there
#' are not more definitions than elements in the list.
#'
#' If no renaming definition is given the input list is returned. If you try
#' to rename elements not present in the list nothing happens; unless you
#' provide more renaming definitions than elements in the list, in that case
#' an error is raised.
#'
#' @return The list provided in `in_list` with elements renamed according to
#' the definition in `...`.
#'
#' @export
#' @examples
#' my_list <- list(a = 1, b = 2, c = 3)
#' list_rename(my_list, AAA = "a", CCC = "c")
list_rename <- function(in_list, ...) {
    expr <- rlang::expr(c(...))
    selection <- tidyselect::eval_select(expr, in_list)

    if (length(selection) == 0) {
        return(in_list)
    }

    if (length(selection) > length(in_list)) {
        stop("Too many renaming definitions.")
    }

    names(in_list)[selection] <- names(selection)

    in_list
}
