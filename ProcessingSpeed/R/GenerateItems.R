# A function that generates a editable amount of items expressed in
# numbers that can later be used to translate to symbols for the symbol
# search subtest
GenerateItems <- function(items) {

  # setting up some variables
  i <- 0
  half_items <- ceiling(items/2)
  all_diff <- list()
  six_diff <- NULL
  one_same <- list()
  correct_numbers <- numeric()

  # for loop to generate half of the items to be all different (the answer
  # to these items will be "no"), and half of the items to have 1 of the
  # first two numbers repeated in the next 5 numbers, the answers to
  # these items will be the number that is repeated, and this is therefore
  # saved in a different variable called "correct_numbers"
  for (i in 1:half_items) {
    all_diff[[i]] <- sample(1:19,7)
    six_diff <- sample(1:19,6)
    one_same[[i]] <- c(sample(six_diff[2:6], 1),six_diff)
    correct_numbers[i] <- one_same[[i]][1]
  }

  # All of the items that have a repeating number now have that number at
  # the start, but it should be either the first or the second number
  # together with one of the last 5. The next part switches the first two
  # numbers of half of these items.
  switch_num <- round(length(one_same)/2)
  switch_these <- sample(length(one_same),switch_num)
  j <- NULL
  first_num <- numeric()
  for (j in 1:switch_num) {
    first_num[j] <- one_same[[switch_these[j]]][1]
    one_same[[switch_these[j]]][1] <- one_same[[switch_these[j]]][2]
    one_same[[switch_these[j]]][2] <- first_num[j]
  }

  # Now putting the two item lists together in a random order, by first
  # putting them in one ordered list (all congruent items first and all
  # incongruent items after that), and then generating a random order. Then
  # using this order putting them in a random order.
  all_in_order <- c(one_same, all_diff)
  item_order <- sample(1:items, items)
  items_list <- list()
  l <- 0
  for (l in 1:items) {
    items_list[[l]] <- all_in_order[[item_order[l]]]
  }

  # Also using this order to get the right answers in the right order.
  # If the item is a 'incongruent' item, the answer will be coded as 0.
  answers_combin <- c(correct_numbers,rep(0,length(all_diff)))
  correct_answers <- numeric()
  for (l in 1:items) {
    correct_answers[l] <- answers_combin[[item_order[l]]]
  }

  # making a clear list of the two things to return, namely the items and the
  # correct answers
  names(items_list) <-   paste("item",1:items,sep = "_")
  items_answers <- list("items" = items_list, answers = correct_answers)

  return(items_answers)
}

