#' Bind list elements together.
#'
#' @param in_list The list to work on.
#' @param ... A selection of elements to bind together.
#' @param what Either 'rows' or 'cols'.
#' @param name Optional name for the resulting element.
#'
#' @details
#' The element to bind together must be compatible in the dimension you
#' want to bind them together, if not there will either be an error or an
#' unexpected result.
#'
#' The `what` parameter specifies whether to call `rbind` or `cbind` on the
#' selected elements.
#'
#' Using `name` you can optionally specify a new name for the result. It
#' will be in the position of the first selected element, while the other
#' selected elements will be removed from the input list.
#'
#' @return A list with the selected elements bound together as specified.
#' @export
#' @examples
#' dfl <- list(data.frame(idx = 1:20, y = rnorm(20)),
#'             data.frame(idx = 21:40, y = rnorm(20)),
#'             data.frame(idx = 41:60, y = rnorm(20)))
#' list_bind(dfl, 1, 2, 3)
list_bind <- function(in_list, ..., what = "rows", name = NULL) {
   unnamed_input <- ifelse(is.null(names(in_list)), TRUE, FALSE)
   idx <- selection(in_list, c(...), enumerate = unnamed_input)
   use_list <- in_list[idx]
   if (what == "rows") {
       fn <- "rbind"
   } else if (what == "cols") {
       fn <- "cbind"
   } else {
       stop("'what' must be 'rows' or 'cols'")
   }
   bind_result <- do.call(fn, use_list)
   in_list[[idx[1]]] <- bind_result
   if (!is.null(name)) {
       names(in_list)[idx[1]] <- name
   }
   in_list[idx[-1]] <- NULL
   in_list
}


#' Bind all elements together and extract them.
#'
#' @param in_list The list to work on.
#' @param what Either 'rows' or 'cols'
#'
#' @details This a convenient wrapper around `list_bind` which selects
#' everything in the list and extracts the result.
#'
#' @return All elements in the list bound together.
#' @export
#' @examples
#' dfl <- list(data.frame(idx = 1:20, y = rnorm(20)),
#'             data.frame(idx = 21:40, y = rnorm(20)),
#'             data.frame(idx = 41:60, y = rnorm(20)))
#' list_bind_all(dfl)
list_bind_all <- function(in_list, what = "rows") {
    in_list |>
        list_bind(tidyselect::everything(), what = what) |>
        list_extract(1)
}
