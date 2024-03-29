% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_insert.R
\name{list_insert}
\alias{list_insert}
\alias{list_append}
\alias{list_prepend}
\title{Insert an element into a list.}
\usage{
list_insert(in_list, item, index, name = NULL, pad = FALSE)

list_append(in_list, item, name = NULL)

list_prepend(in_list, item, name = NULL)
}
\arguments{
\item{in_list}{The list to work on.}

\item{item}{The item to add to the list.}

\item{index}{The index to insert at.}

\item{name}{Optional name for the new item.}

\item{pad}{Add `NULL` elements for too large indices?}
}
\value{
A list, the same as `in_list` but with `item` added at `index`.
}
\description{
Insert an element into a list.
}
\details{
The `index` behaves in the way that everything including the specified
index will be moved one position forward.
Thus, if you insert at index 2, the old item at index 2 will be moved to
index 3.
If `index` is larger than the length of `in_list` the default behaviour
is to just add the new item to the end of the list, however if
you specify `pad = TRUE` then as many `NULL` elements as needed are
added to the list to insert `item` at the specified location.

The functions `list_append` and `list_prepend` exist as a simple short-cut
for appending and prepending to a list.
}
\examples{
my_list <- list(foo1 = 1:10, foo2 = LETTERS[1:10])
list_insert(my_list, rnorm(3), 2, name = "bar")
}
