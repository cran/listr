describe("list_reaname", {
    test_list <- generate_test_list()
    it("can rename named elements in a list", {
        names_pre <- names(test_list)
        test_list <- list_rename(test_list, new1 = "element1",
                                 dfdf = "element_df1")
        names_post <- names(test_list)
        expect_false(any(c("new1", "dfdf") %in% names_pre))
        expect_true(all(c("new1", "dfdf") %in% names_post))
    })

    it("will return the input list if no naming definition is given", {
        test_list_2 <- list_rename(test_list)
        expect_equal(test_list, test_list_2)
    })
})
