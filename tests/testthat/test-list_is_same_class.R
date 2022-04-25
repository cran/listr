test_list_unequal <- generate_test_list()
test_list_equal <- list(
    c(1:10), c(11:20), c(5:25)
)
test_list_compatible <- list(
    df1 = data.frame(x = rnorm(10), y = rnorm(10)),
    df2 = tibble::tibble(a = rnorm(10), b = letters[1:10])
)

describe("list_is_same_class", {
    it("returns FALSE if classes are mixed", {
        expect_false(list_is_same_class(test_list_unequal))
    })

    it("returns FALSE if classes are compatible but unequal", {
        expect_false(list_is_same_class(test_list_compatible))
    })

    it("returns TRUE if classes are equal", {
        expect_true(list_is_same_class(test_list_equal))
    })
})


describe("list_is_compatible_class", {
    it("returns FALSE if classes are mixed", {
        expect_false(list_is_compatible_class(test_list_unequal))
    })

    it("returns TRUE if classes are compatible but unequal", {
        expect_true(list_is_compatible_class(test_list_compatible))
    })

    it("returns TRUE if classes are equal", {
        expect_true(list_is_compatible_class(test_list_equal))
    })
})
