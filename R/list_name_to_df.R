#' Add the names of list items to data frames.
#'
#' @param in_list The list to work on. Must have names.
#' @param column_name The name of the column to add to the data frames.
#' @param skip_non_df Whether to skip items that are not data frames.
#'
#' @details
#' With `column_name` you can specify the name the new columns in the
#' data.frames should have. The default is `.group`.
#'
#' Using `skip_non_df` you can specify to omit elements from the input list
#' that are not data frames. If FALSE an error will be thrown if elements are
#' present that are not data frames. If TRUE (the default) then items that are
#' not data frames will be ignored and remain unchanged.
#'
#' @return The input list with the name of the list item added in a new column
#' for all data frames.
#'
#' @export
#' @examples
#' my_list <- list(group1 = data.frame(x = 1:10, y = rnorm(10)),
#'                 group2 = data.frame(x = 1:10, y = rnorm(10)))
#' list_name_to_df(my_list)
list_name_to_df <- function(in_list, column_name = ".group",
                            skip_non_df = TRUE) {
    if (is.null(names(in_list))) {
        stop("List has no names.")
    }
    if (!skip_non_df) {
        if (!all(sapply(in_list, inherits, "data.frame"))) {
            stop(paste("List has non data.frame elements.",
                       "Consider using skip_non_df = TRUE."))
        }
    }
    column_name <- as.character(rlang::ensym(column_name))
    in_names <- names(in_list)
    mod_list <- lapply(seq_len(length(in_list)), function(x) {
        if (inherits(in_list[[x]], "data.frame")) {
            new_df <- in_list[[x]]
            new_df[[column_name]] <- in_names[x]
            new_df
        } else {
            in_list[[x]]
        }
    })
    names(mod_list) <- in_names
    mod_list
}
