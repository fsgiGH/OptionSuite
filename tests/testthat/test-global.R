test_that("default global settings are loaded correctly", {
  expect_equal(get_global_r(), 0.05)
  expect_equal(get_day_count_convention(), "calendar/365")
  expect_equal(get_min_reliable_ttm(), 0.0014)
  expect_false(get_use_commission())  # expect_false is clearer for FALSE
})

test_that("can set and get global risk-free rate", {
  old <- get_global_r()
  set_global_r(0.03)
  expect_equal(get_global_r(), 0.03)
  set_global_r(old)  # restore
  expect_equal(get_global_r(), old)
})

test_that("can set and get day-count convention", {
  old <- get_day_count_convention()
  set_day_count_convention("trading/252")
  expect_equal(get_day_count_convention(), "trading/252")
  set_day_count_convention(old)
  expect_equal(get_day_count_convention(), old)
})

test_that("can set and get minimum reliable TTM", {
  old <- get_min_reliable_ttm()
  set_min_reliable_ttm(0.002)
  expect_equal(get_min_reliable_ttm(), 0.002)
  set_min_reliable_ttm(old)
  expect_equal(get_min_reliable_ttm(), old)
})

test_that("can set and get commission flag", {
  old <- get_use_commission()
  set_use_commission(TRUE)
  expect_true(get_use_commission())
  set_use_commission(FALSE)
  expect_false(get_use_commission())
  set_use_commission(old)  # restore even though we know it's FALSE
})

test_that("date_to_ttm handles various input types", {
  # Numeric input
  expect_equal(date_to_ttm(0.5), 0.5)
  
  # Date input
  d1 <- as.Date("2026-06-18")
  d2 <- as.Date("2026-03-15")
  ttm <- date_to_ttm(d1, d2)
  expected <- as.numeric(difftime(d1, d2, units = "days")) / 365
  expect_equal(ttm, expected)
  
  # Character input
  expect_equal(date_to_ttm("2026-06-18", "2026-03-15"), expected)
  
  # Mixed Date/character
  expect_equal(date_to_ttm(d1, "2026-03-15"), expected)
  expect_equal(date_to_ttm("2026-06-18", d2), expected)
})

test_that("date_to_ttm validates inputs", {
  expect_error(date_to_ttm("invalid-date"), 
               "Cannot parse expiry string: 'invalid-date'")
  expect_error(date_to_ttm(as.Date("2026-06-18"), "invalid-date"), 
               "Cannot parse valuation_date string: 'invalid-date'")  # Updated!
  expect_error(date_to_ttm(TRUE), 
               "expiry must be Date, character")
})