% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_is_same_class.R
\name{list_is_same_class}
\alias{list_is_same_class}
\alias{list_is_compatible_class}
\title{Check whether all elements of a list have the same class.}
\usage{
list_is_same_class(list)

list_is_compatible_class(list)
}
\arguments{
\item{list}{The list to check.}
}
\value{
Boolean value.
}
\description{
This is a convenience function to check whether all elements
of a list have the same class. It will only return TRUE if all elements
in a list are of the exact same class. This means that if a list has
two vectors TRUE will only be returned if they have the same mode or in
case list has elements of compatible classes like data.frame and tbl.df
the result will be false.

For the latter case there is `list_is_compatible_class` that checks
whether elements of vectors of classes overlap. Note that this does not
necessarily mean that elements can be safely combined, this depends on
the respective implementations.
}
\examples{
test_list_false <- list(c(1, 2), c(3, 4), c("abc", "def"))
list_is_same_class(test_list_false)

test_list_true <- list(c(1, 2), c(3, 4))
list_is_same_class(test_list_true)
}
