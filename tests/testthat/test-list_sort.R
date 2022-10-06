describe("list_sort", {
    test_list <- generate_test_list()
    test_list <- list_flatten(test_list)

    it("sorts a list accoring to a function", {
        sorted_test <- list_sort(test_list, length)
        sorted_lens <- lengths(sorted_test)
        expect_true(all(diff(sorted_lens) >= 0))
    })

    it("can optionally sort in descending order", {
        sorted_test <- list_sort(test_list, length, descending = TRUE)
        sorted_lens <- lengths(sorted_test)
        expect_true(all(diff(sorted_lens) <= 0))
    })

    it("can work with any kind of function as long as it returns a number", {
        my_sort_fun <- function(data) {
            if (is.data.frame(data)) {
                sort_val <- 999
            } else if (is.character(data)) {
                sort_val <- -999
            } else {
                sort_val <- 0
            }
            sort_val
        }
        sorted_test <- list_sort(test_list, my_sort_fun)
        expect_true(is.character(sorted_test[[1]]))
        expect_true(is.data.frame(sorted_test[[length(sorted_test)]]))
    })

    it("can put values where the sort function returns NA first or last", {
        use_list <- list(
            1:10,
            11:21,
            c(NA, NA, NA)
        )
        sort_result1 <- list_sort(use_list, mean, use_na = "first")
        expect_true(all(is.na(sort_result1[[1]])))
        sort_result2 <- list_sort(use_list, mean, use_na = "last")
        expect_true(all(is.na(sort_result2[[3]])))
    })
})
