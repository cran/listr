describe("list_bind", {
    test_list <- list(
        df1 = data.frame(x = 1:10, y = rnorm(10)),
        df2 = data.frame(x = 11:20, y = rnorm(10)),
        df3 = data.frame(x = 21:30, y = rnorm(10))
    )

    it("can bind rows", {
        bound <- list_bind(test_list, 1:3)
        expect_length(bound, 1)
        expect_equal(ncol(test_list[[1]]), ncol(bound[[1]]))
        expect_equal(sum(sapply(test_list, nrow)), nrow(bound[[1]]))
    })

    it("can bind cols", {
        bound <- list_bind(test_list, 1:3, what = "cols")
        expect_length(bound, 1)
        expect_equal(nrow(test_list[[1]]), nrow(bound[[1]]))
        expect_equal(sum(sapply(test_list, ncol)), ncol(bound[[1]]))
    })

    it("it can work on a selection of items", {
        bound <- list_bind(test_list, 2:3)
        expect_length(bound, 2)
        expect_equal(sum(sapply(test_list[2:3], nrow)), nrow(bound[[2]]))
        expect_equal(test_list[[1]], bound[[1]])
    })

    it("can assign name to the new item", {
        bound <- list_bind(test_list, 1:3, name = "foo")
        expect_equal(names(bound)[1], "foo")
    })

    it("throws an error if what is not rows or cols", {
        expect_error(list_bind(test_list, 1, 2, what = "meow"),
                     "'what' must be 'rows' or 'cols'")
    })
})
