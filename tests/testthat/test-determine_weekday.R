test_that("determine_weekday() correctly identifies weekdays", {
  expect_equal(determine_weekday("2025-03-16"), "Sunday")
  expect_equal(determine_weekday("2025-03-17"), "Monday")
})

test_that("determine_weekday() handles various date formats", {
  expect_equal(determine_weekday("16/03/2025", language = "en"), "Sunday")
  expect_equal(determine_weekday("March 16, 2025"), "Sunday")
})

test_that("determine_weekday() supports language options", {
  expect_equal(determine_weekday("2025-03-16", language = "fr"), "dimanche")  # French
  expect_equal(determine_weekday("2025-03-16", language = "es"), "domingo")   # Spanish
})

test_that("determine_weekday() returns NA for invalid dates", {
  expect_true(is.na(determine_weekday("invalid-date")))
  expect_true(is.na(determine_weekday("31/02/2025")))  # Impossible date
})

test_that("determine_weekday() warns for unsupported languages", {
  expect_message(determine_weekday("2025-03-16", language = "de"),
                 "Unsupported language. Using default: English (en).")
})
