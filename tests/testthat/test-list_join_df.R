describe("list_join_df", {
    testlist <- list(
        df1 = data.frame(id = 1:20, x = rnorm(20)),
        df2 = data.frame(id = 6:25, y = rnorm(20)),
        df3 = data.frame(id = 11:30, z = rnorm(20)),
        nodf = letters[1:10]
    )

    it("fails by default if non-df elements are present", {
        expect_error(list_join_df(testlist))
    })

    it("keeps only common elements by default", {
        joined <- list_join_df(testlist, skip_non_df = TRUE)
        expect_equal(nrow(joined), 10)
        expect_equal(ncol(joined), 4)
    })

    it("can optinally keep elements from the first, last or all dfs", {
        left <- list_join_df(testlist, join_type = "left", skip_non_df = TRUE)
        right <- list_join_df(testlist, join_type = "right", skip_non_df = TRUE)
        full <- list_join_df(testlist, join_type = "full", skip_non_df = TRUE)

        expect_equal(nrow(left), 20)
        expect_equal(nrow(right), 20)
        expect_equal(testlist$df1$id, left$id)
        expect_equal(testlist$df3$id, right$id)
        expect_equal(nrow(full), 30)
        expect_true(all(c(ncol(left), ncol(right), ncol(full)) == 4))
    })
})
