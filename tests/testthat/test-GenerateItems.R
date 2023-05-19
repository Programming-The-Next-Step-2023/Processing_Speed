library('ProcessingSpeed')

context('Test General GenerateItems Functionality')

test_that("compute if GenerateItems generates a list of correct length", {
  some_items <- GenerateItems(nr_items = 10)

  expect_equal(length(some_items$items), nr_items)
  expect_equal(length(some_items$items$item_1), 7)
})
