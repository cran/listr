#' Sort a list.
#'
#' @param in_list The list to work on.
#' @param sort_fun The function to sort the list by.
#' @param descending Boolean, if TRUE items will be sorted in descending order.
#' @param use_na String, one of `drop`, `first` or `last`.
#'
#' @details
#' The list will be sorted according to the return value of `sort_fun`. The
#' function must return a numeric value that will then be used to order the
#' list items.
#'
#' If the function returns `NA` the sorting will depend on the string
#' specified to `use_missing`. The default is to drop these values, but they can
#' optionally be put first or last in the sorted list.
#'
#' If the sorting function returns `NA` for some items and `use_na` is `first`
#' or `last` then the order of the `NA` items will most likely be the same
#' as in `in_list` (but this cannot be guaranteed). The same is true for
#' ties.
#'
#' @return A list, `in_list` sorted according to `sort_fun`.
#'
#' @export
#' @examples
#' my_list <- list(data.frame(x = 1:10),
#'                 data.frame(x = 1:5, y = rnorm(5)),
#'                 data.frame(x = 1:20, y = rnorm(20), z = LETTERS[1:20]))
#' list_sort(my_list, nrow)
#' list_sort(my_list, function(x) sum(x$x), descending = TRUE)
list_sort <- function(in_list, sort_fun, descending = FALSE, use_na = "drop") {
    stopifnot(use_na %in% c("drop", "first", "last"))
    order_idx <- lapply(in_list, sort_fun)
    res_len <- lengths(order_idx)
    if (any(res_len > 1)) {
        stop("sort_fun should return only one numeric value")
    }
    if (any(res_len == 0)) {
        order_idx[res_len == 0] <- NA_real_
    }
    na_opt <- switch(use_na,
        "drop" = NA,
        "first" = FALSE,
        "last" = TRUE
    )
    order_vec <- unlist(order_idx)
    order_vec <- order(order_vec, na.last = na_opt, decreasing = descending)
    in_list[order_vec]
}
