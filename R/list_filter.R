#' Filter a list.
#'
#' @param in_list The list to filter.
#' @param filter_fun The function to use for filtering.
#'
#' @details `filter_fun` must evaluate to TRUE or FALSE for filtering, where
#' elements returning TRUE are kept.
#'
#' @return A list, `in_list` but filtered according to `filter_fun.`
#' @export
#' @examples
#' my_list <- list(aa = 1:3, bb =  1:4, cc = 2:5)
#' list_filter(my_list, function(x) min(x) == 1)
#' list_filter(my_list, function(x) max(x) > 3)
list_filter <- function(in_list, filter_fun) {
  to_keep <- lapply(in_list, filter_fun)
  to_keep <- unlist(to_keep)
  # complete.cases does omit NULL values, so it seems this clunky
  # statement is necessary?
  to_keep[is.na(to_keep) | is.null(to_keep) | is.nan(to_keep)] <- FALSE
  if (!is.logical(to_keep)) {
    stop("Filter function must return logical values.")
  }
  in_list[to_keep]
}
