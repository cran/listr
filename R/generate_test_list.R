#' Generate test data.
#'
#' @return A list with some data.
#' @noRd
#' @keywords internal
generate_test_list <- function() {
    list(
        element1 = stats::rnorm(40),
        element2 = stats::rnorm(20),
        abc = sample(letters, replace = TRUE, size = 20),
        def = sample(LETTERS, replace = TRUE, size = 30),
        ghi = list(
            stats::rnorm(20),
            stats::rnorm(30),
            list(
                xyz = stats::rnorm(5),
                zyx = stats::rnorm(5),
                yxz = stats::rnorm(10)
            ),
            stats::rnorm(10)
        ),
        c(NA, NA, NA),
        c(1:20),
        c(20:1),
        element_df1 = data.frame(
            id = sample(100, size = 40),
            a = stats::rnorm(40),
            b = stats::rnorm(40),
            c = stats::rnorm(40)
        ),
        element_df2 = data.frame(
            id = sample(100, size = 40),
            d = stats::rnorm(40),
            b = stats::rnorm(40),
            h = stats::rnorm(40)
        ),
        element_df3 = data.frame(
            id = sample(100, size = 40),
            j = stats::rnorm(40),
            k = stats::rnorm(40),
            c = stats::rnorm(40)
        )
    )
}
