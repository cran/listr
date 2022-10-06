describe("list_filter", {
  test_list <- generate_test_list()

  it("filters according to a function", {
    tl_classes <- sapply(test_list, class)
    pre <- all(tl_classes == "data.frame")
    filtered <- list_filter(test_list, is.data.frame)
    fl_classes <- sapply(filtered, class)
    post <- all(fl_classes == "data.frame")
    expect_true(post)
    expect_false(pre == post)
    expect_lt(length(filtered), length(test_list))
  })

  it("throws an error if the function does not return logical values", {
    expect_error(
      list_filter(test_list, length),
      "Filter function must return logical values."
    )
  })

  it("ignores NA/NaN/NULL results of the filter function", {
    small_list <- list(a = 1:3, b = letters[1:5], c = 10:12)
    expect_warning(
      result <- list_filter(small_list, function(x) sum(as.numeric(x)) < 100))
    expect_length(result, 2)
  })

})
