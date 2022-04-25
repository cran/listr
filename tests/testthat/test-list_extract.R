describe("list_extract", {
    test_list <- generate_test_list()

    it("extracts a single element from a list", {
        item <- list_extract(test_list, element_df1)
        expect_equal(item, test_list[["element_df1"]])
    })

    it("throws an error when more than one element is selected", {
        expect_error(list_extract(test_list, 1, 2))
    })

    it("can select by position as well", {
        item <- list_extract(test_list, 5)
        expect_equal(item, test_list[[5]])
    })
})
