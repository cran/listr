describe("list_remove", {
    test_list <- generate_test_list()

    it("removes elements from a list", {
        in_len <- length(test_list)
        new_list <- list_remove(test_list, element_df1, 1, "abc")
        out_len <- length(new_list)
        expect_equal(out_len, in_len - 3)
        expect_false(any(c("element_df1", "abc") %in% names(new_list)))
    })

    it("throws an error if elements to be removed do not exit in the list", {
        expect_error(list_remove(test_list, "foo"))
    })
})
