context("Testing surname utility functions.")

test_that("Surname utility functions work correctly.", {
  voter_file <- data.frame(
    surname = c(
      "JOHNSON",
      "JOHNSON,?`'",
      "O'JOHNSON",
      "JOHNSON-WASHINGTON",
      "DE JOHNSON-WASHINGTON",
      "DE JOHNSON WASHINGTON",
      "O'DE LA JOHNSON-WASHINGTON SR."
    )
  )

  # Check word count function
  word_counts <- get_word_count(
    voter_file = voter_file,
    surname_col = "surname"
  )
  expected_counts <- c(1, 1, 1, 2, 3, 3, 5)
  testthat::expect_equal(word_counts, expected_counts)

  # Check unique characters
  unique_characters <- get_unique_special_characters(
    voter_file = voter_file,
    surname_col = "surname"
  )
  expected_characters <- c(" ", ",", "?", "-", "`", "'", ".")
  testthat::expect_equal(sort(unique_characters), sort(expected_characters))

  # Check special character surnames
  special_character_surnames <- get_special_character_surnames(
    voter_file = voter_file,
    surname_col = "surname"
  )
  expected_surnames <- voter_file[-1, ]
  testthat::expect_equal(special_character_surnames, expected_surnames)

  # Check special characters stripped
  stripped_special_characters <- strip_special_characters(
    voter_file = voter_file,
    surname_col = "surname",
    replace = ""
  )
  expected_stripped_voter_file <- data.frame(
    surname = c(
      "JOHNSON",
      "JOHNSON",
      "OJOHNSON",
      "JOHNSONWASHINGTON",
      "DEJOHNSONWASHINGTON",
      "DEJOHNSONWASHINGTON",
      "ODELAJOHNSONWASHINGTONSR"
    )
  )
  testthat::expect_equal(
    stripped_special_characters,
    expected_stripped_voter_file
  )

  # Check multi-barreled surnames
  multi_barreled_surnames <- get_multi_barreled_surnames(
    voter_file = voter_file,
    surname_col = "surname",
  )
  expected_multi_surnames <- data.frame(
    surname = c(
      "JOHNSON-WASHINGTON",
      "DE JOHNSON-WASHINGTON",
      "DE JOHNSON WASHINGTON",
      "O'DE LA JOHNSON-WASHINGTON SR."
    )
  )
  testthat::expect_equal(multi_barreled_surnames, expected_multi_surnames)

  # Test surname match function
  matches_no_special <- surname_match(
    voter_file = voter_file,
    surname_col = "surname",
    strip_special = TRUE
  )
  matches_w_special <- surname_match(
    voter_file = voter_file,
    surname_col = "surname",
    strip_special = FALSE
  )
  expected_no_special <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  expected_w_special <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  testthat::expect_equal(matches_no_special, expected_no_special)
  testthat::expect_equal(matches_w_special, expected_w_special)

  # Test surname summary function
  expect_error(
    surname_summary(voter_file = voter_file, surname_col = "surname"),
    NA
  )
})
