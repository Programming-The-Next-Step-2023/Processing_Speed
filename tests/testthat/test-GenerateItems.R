library('ProcessingSpeed')

test_that("compute if GenerateItems generates a list of correct length", {
  nr_items <- 10
  some_items <- GenerateItems(nr_items = nr_items)
  # see if the number of items generated is the correct amount
  expect_equal(length(some_items$items), nr_items)
})

test_that("compute if all items are vectors of length 7", {
  nr_items <- 10
  some_items <- GenerateItems(nr_items = nr_items)
  # first make a vector of the length of each individual item
  length_items <- numeric()
  for (i in 1:nr_items) {
    length_items[i] <-length(some_items$items[[i]])
  }
  # Then check if these are all length 7
  expect_equal(length_items, rep(7, nr_items))
})

test_that("compute if half of the items are congruent/incongruent - even", {
  even_nr <- 20
  even_items <- GenerateItems(nr_items = even_nr)
  # first make a vector of the nr of unique numbers in each item
  nr_unique <- numeric()
  for (i in 1:even_nr) {
    nr_unique[i] <-length(unique(even_items$items[[i]]))
  }
  # then calculate how many have 6 (congruent items, 1 repeating number) and
  # how many have 7 (incongruent items, all numbers different)
  congr <- sum(nr_unique == 6)
  incongr <- sum(nr_unique == 7)

  # if the nr_items was even, the congruent and incongruent items should be
  # exactly equal.
    expect_equal(congr, incongr)
})

test_that("compute if half of the items are congruent/incongruent - odd", {
  # same test for odd number of items
  odd_nr <- 21
  odd_items <- GenerateItems(nr_items = odd_nr)
  # vector of nr of unique numbers in each item
  nr_unique <- numeric()
  i <- numeric()
  for (i in 1:odd_nr) {
    nr_unique[i] <-length(unique(odd_items$items[[i]]))
  }
  # then calculate how many have 6 and 7
  congr <- sum(nr_unique == 6)
  incongr <- sum(nr_unique == 7)

  # When nr_items is odd, either the congruent or incongruent items get one
  # extra item in comparison to the other.
    expect_equal(abs(congr-incongr), 1)
})
