#' Check whether all elements of a list have the same class.
#'
#' @description This is a convenience function to check whether all elements
#' of a list have the same class. It will only return TRUE if all elements
#' in a list are of the exact same class. This means that if a list has
#' two vectors TRUE will only be returned if they have the same mode or in
#' case list has elements of compatible classes like data.frame and tbl.df
#' the result will be false.
#'
#' For the latter case there is `list_is_compatible_class` that checks
#' whether elements of vectors of classes overlap. Note that this does not
#' necessarily mean that elements can be safely combined, this depends on
#' the respective implementations.
#'
#' @param list The list to check.
#' @return Boolean value.
#' @export
#' @examples
#' test_list_false <- list(c(1, 2), c(3, 4), c("abc", "def"))
#' list_is_same_class(test_list_false)
#'
#' test_list_true <- list(c(1, 2), c(3, 4))
#' list_is_same_class(test_list_true)
list_is_same_class <- function(list) {
    list_compare_class(list, mode = "equality")
}

#' @rdname list_is_same_class
#' @export
list_is_compatible_class <- function(list) {
    list_compare_class(list, mode = "compatibility")
}

#' @noRd
#' @keywords internal
list_compare_class <- function(list, mode = "equality") {
    classes <- lapply(list, class)
    names(classes) <- NULL
    compare_to <- classes[[1]]
    if (mode == "equality") {
        same_class <- lapply(classes, function(x) all(compare_to == x))
    } else if (mode == "compatibility") {
        same_class <- lapply(classes, function(x) any(compare_to %in% x))
    } else {
        stop("Mode must be equality or combatibility.")
    }
    all(unlist(same_class))
}
