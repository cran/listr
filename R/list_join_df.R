#' Join a list of data frames on a common index.
#'
#' @param in_list A list of data frames.
#' @param join_type A string specifying the join strategy to use. Must be
#' one of "inner", "left", "right" or "full".
#' @param by Optional vector of strings specifying columns to merge by.
#' @param skip_non_df Should elements that are not data.frames be skipped?
#'
#' @details
#' Using `join_type` you can specify how to join the data. The default
#' `inner` will keep only observations present in all data frames. `left` will
#' keep all observations from the first data frame in the list and merge the
#' matching items from the rest, while `right` will keep all observations from
#' the last data frame in the list. Using `full` will keep all observations.
#'
#' If `by` is not supplied, then data frames will be merged on columns with
#' names they all have. Otherwise merging is done on the specified columns.
#'
#' Using `skip_non_df` you can specify to omit elements from the input list
#' that are not data frames. If FALSE (the default) an error will be thrown
#' if elements are present that are not data frames.
#'
#' @return A data frame.
#' @export
#' @examples
#' dfl <- list(data.frame(idx = sample(100, 30), x = rnorm(30)),
#'             data.frame(idx = sample(100, 30), y = rnorm(30)),
#'             data.frame(idx = sample(100, 30), z = rnorm(30)))
#' list_join_df(dfl)
list_join_df <- function(in_list, join_type = "inner", by = NULL,
                         skip_non_df = FALSE) {
    is_df <- sapply(in_list, inherits, "data.frame")

    if (!skip_non_df & !all(is_df)) {
        stop("Some elements in the list are not data frames. Consider using
             skip_non_df = TRUE.")
    } else {
        in_list <- in_list[is_df]
    }

    if (join_type == "inner") {
        ljoin <-  FALSE
        rjoin <-  FALSE
    } else if (join_type == "left") {
        ljoin <-  TRUE
        rjoin <-  FALSE
    } else if (join_type == "right") {
        ljoin <- TRUE
        rjoin <- FALSE
        in_list <- rev(in_list)
    } else if (join_type == "full") {
        ljoin <- TRUE
        rjoin <- TRUE
    } else {
        stop("join_type must be one of 'inner', 'left', 'right' or 'full'.")
    }

    mergefun <- ifelse(
        is.null(by),
        function(x, y) merge(x, y, all.x = ljoin, all.y = rjoin),
        function(x, y) merge(x, y, by = by, all.x = ljoin, all.y = rjoin))

    Reduce(function(x, y) mergefun(x, y), in_list)
}
