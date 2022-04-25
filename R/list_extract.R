#' Extract an element from a list using tidy selection.
#'
#' This is pretty much an equivalent of calling `[[` on a list, but allows
#' for cleaner use inside pipes.
#'
#' @param in_list The list to extract an element from.
#' @param ... A selection of what to extract. Must be a single element.
#'
#' @return The selected list element.
#' @export
#' @examples
#' my_list <- list(rnorm(20), data.frame(x = 1:10, y = rnorm(10)), letters[1:5])
#' list_extract(my_list, 3)
list_extract <- function(in_list, ...) {
    selection <- selection(in_list, enumerate = TRUE, c(...))
    if (length(selection) > 1) {
        stop("Can only extract a single element.")
    }
    in_list[[selection]]
}
