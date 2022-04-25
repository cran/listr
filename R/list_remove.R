#' Remove elements from a list.
#'
#' @param in_list The list to remove elements from.
#' @param ... Names or numeric positions of elements to remove.
#'
#' @return The list with the specified elements removed.
#' @export
#'
#' @examples
#' my_list <- list(a = rnorm(10), b = rnorm(10), c = rnorm(10))
#' list_remove(my_list, b)
list_remove <- function(in_list, ...) {
    selection <- selection(in_list, c(...))
    in_list[-selection]
}
