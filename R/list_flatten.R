#' Flatten nested lists.
#'
#' `list_flatten()` works recursively through an input list and puts all
#' elements of nested list to the top level. If there are no nested lists
#' then the input is returned unchanged.
#'
#' Using `max_depth` you can control whether to flatten all nested lists.
#' Negative values will cause all nested lists to be flattened, positive
#' depths will limit the depth of the recursion.
#'
#' @param in_list The list to flatten
#' @param max_depth Maximum depth to recurse into.
#'
#' @return A list without nested lists.
#' @export
#' @examples
#' my_list <- list(a = list(1, 2, 3), b = list(4, 5, 6))
#' list_flatten(my_list)
list_flatten <- function(in_list, max_depth = -1) {
    is_sublist <- function(chk_list) {
        sapply(chk_list, inherits, "list")
    }

    sub_list <- is_sublist(in_list)
    if (!any(sub_list) | max_depth == 0) {
        return(in_list)
    }

    list_idx <- which(sub_list)
    new_length <- length(in_list) + sum((lengths(in_list)[list_idx] - 1))
    out_list <- vector(mode = "list", length = new_length)

    in_list_names <- names(in_list)
    if (is.null(in_list_names)) {
        in_list_names <- paste0("X", seq_len(length(in_list)))
    }
    out_list_names <- vector(mode = "character", length = new_length)

    in_list_idx <- 1
    sub_list_idx <- 1

    for (idx in seq(1, length(out_list))) {
        if (inherits(in_list[[in_list_idx]], "list")) {
            out_list[[idx]] <- in_list[[in_list_idx]][[sub_list_idx]]
            sub_item_name <- names(in_list[[in_list_idx]][sub_list_idx])
            if (is.null(sub_item_name)) {
                out_list_names[idx] <- paste0(
                    in_list_names[in_list_idx], "_", sub_list_idx)
            } else {
                out_list_names[idx] <- paste0(
                    in_list_names[in_list_idx], "_", sub_item_name
                )
            }
            if (sub_list_idx == length(in_list[[in_list_idx]])) {
                sub_list_idx <- 1
                in_list_idx <- in_list_idx + 1
            } else {
                sub_list_idx <- sub_list_idx + 1
            }
        } else {
            out_list[[idx]] <- in_list[[in_list_idx]]
            out_list_names[idx] <- in_list_names[in_list_idx]
            in_list_idx <- in_list_idx + 1
        }
    }

    names(out_list) <- out_list_names

    list_flatten(out_list, max_depth = max_depth - 1)
}
