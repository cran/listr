#' Insert an element into a list.
#'
#' @param in_list The list to work on.
#' @param item The item to add to the list.
#' @param index The index to insert at.
#' @param name Optional name for the new item.
#' @param pad Add `NULL` elements for too large indices?
#'
#' @details
#' The `index` behaves in the way that everything including the specified
#' index will be moved one position forward.
#' Thus, if you insert at index 2, the old item at index 2 will be moved to
#' index 3.
#' If `index` is larger than the length of `in_list` the default behaviour
#' is to just add the new item to the end of the list, however if
#' you specify `pad = TRUE` then as many `NULL` elements as needed are
#' added to the list to insert `item` at the specified location.
#'
#' The functions `list_append` and `list_prepend` exist as a simple short-cut
#' for appending and prepending to a list.
#'
#' @return A list, the same as `in_list` but with `item` added at `index`.
#'
#' @export
#' @examples
#' my_list <- list(foo1 = 1:10, foo2 = LETTERS[1:10])
#' list_insert(my_list, rnorm(3), 2, name = "bar")
list_insert <- function(in_list, item, index, name = NULL, pad = FALSE) {
    if (index > length(in_list)) {
        if (!pad) {
           index <- length(in_list) + 1
        }
        new_list <- in_list
        new_list[[index]] <- item
    } else if (index == 1) {
        new_list <- c(list(item), in_list)
    } else {
        part1 <- in_list[1:(index - 1)]
        part2 <- in_list[index:length(in_list)]
        new_list <- c(part1, list(item), part2)
    }
    if (!is.null(name)) {
        names(new_list)[index] <- name
    }
    new_list
}

#' @rdname list_insert
#' @export
list_append <- function(in_list, item, name = NULL) {
    list_insert(in_list, item, index = length(in_list) + 1, name = name)
}

#' @rdname list_insert
#' @export
list_prepend <- function(in_list, item, name = NULL) {
    list_insert(in_list, item, index = 1, name = name)
}
