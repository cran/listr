describe("list_name_to_df", {
    this_list <- list(
        a = data.frame(x = 1:10, y = rnorm(10)),
        b = data.frame(x = 1:10, y = rnorm(10)),
        c = data.frame(x = 1:10, y = rnorm(10))
    )

    this_list_2 <- this_list
    this_list_2[["nondf"]] <- 1

    it("adds the list item's name to each data.frame in the list", {
        new_list <- list_name_to_df(this_list)
        names <- lapply(new_list, colnames)
        expect_true(all(sapply(names, function(x) ".group" %in% x)))
        new_col <- sapply(seq_len(length(new_list)), function(x) {
            names(this_list)[x] == unique(new_list[[x]][[".group"]])
        })
        expect_true(all(new_col))
    })

    it("accepts custom names for the new colum", {
        new_list <- list_name_to_df(this_list, column_name = "foo")
        names <- lapply(new_list, colnames)
        expect_true(all(sapply(names, function(x) "foo" %in% x)))
    })

    it("also accepts bare words for the new column name", {
        new_list <- list_name_to_df(this_list, column_name = foo)
        names <- lapply(new_list, colnames)
        expect_true(all(sapply(names, function(x) "foo" %in% x)))
    })

    it("skips non data.frame elements by default", {
        new_list <- list_name_to_df(this_list_2)
        expect_equal(new_list[["nondf"]], this_list_2[["nondf"]])
        names <- lapply(new_list,
                        function(x) if (inherits(x, "data.frame")) colnames(x))
        empty <- sapply(names, is.null)
        expect_true(all(sapply(names[!empty], function(x) ".group" %in% x)))
    })

    it("stops on non data.frame elemnts if requested", {
        expect_error(list_name_to_df(this_list_2, skip_non_df = FALSE),
                     "List has non data.frame elements.")
    })
})
