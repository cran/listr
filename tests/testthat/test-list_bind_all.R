describe("list_bind_all", {
    test_list <- list(
        df1 = data.frame(x = 1:10, y = rnorm(10)),
        df2 = data.frame(x = 11:20, y = rnorm(10)),
        df3 = data.frame(x = 21:30, y = rnorm(10))
    )

    it("binds everything togehter", {
        result <- list_bind_all(test_list)
        expect_s3_class(result, "data.frame")
        expect_equal(nrow(result), sum(sapply(test_list, nrow)))
    })

    it("also binds columns", {
        result <- list_bind_all(test_list, what = "cols")
        expect_s3_class(result, "data.frame")
        expect_equal(ncol(result), sum(sapply(test_list, ncol)))
    })
})
