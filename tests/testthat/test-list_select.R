describe("list_select", {
    test_list <- generate_test_list()
    it("allows to select elements by name", {
        sel <- list_select(test_list, "abc", "def")
        expect_equal(sel, test_list[c("abc", "def")])
    })

    it("allows to select elements by index", {
        sel <- list_select(test_list, 3, 9)
        expect_equal(sel, test_list[c(3, 9)])
    })

    it("allows to mix both types of selection", {
        sel <- list_select(test_list, 1, "def")
        expect_equal(sel, c(test_list[1], test_list["def"]))
    })
})
