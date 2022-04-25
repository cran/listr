describe("list_flatten", {
    test_list <- generate_test_list()
    it("returns a list without sublists", {
        expect_true(any(sapply(test_list, inherits, "list")))
        flat <- list_flatten(test_list)
        expect_false(any(sapply(flat, inherits, "list")))
    })
})
