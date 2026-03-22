# tests/testthat/test-legs.R
# Tests for leg class functionality

test_that("make_leg validates inputs correctly", {
  # Create a test instrument
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  # Valid leg
  leg <- make_leg(opt, quantity = 5, direction = "buy")
  expect_s3_class(leg, "leg")
  expect_equal(leg$quantity, 5)
  expect_equal(leg$direction, "buy")
  
  # Invalid quantity
  expect_error(make_leg(opt, quantity = -5, direction = "buy"), "quantity > 0")
  expect_error(make_leg(opt, quantity = 0, direction = "buy"), "quantity > 0")
  
  # Invalid direction
  expect_error(make_leg(opt, quantity = 5, direction = "invalid"), 
               "'arg' should be one of")
  
  # Invalid instrument
  expect_error(make_leg("not an instrument", quantity = 5, direction = "buy"),
               "inherits\\(instrument, \"instrument\"\\) is not TRUE")
})

test_that("leg.cash_flow works correctly", {
  # Option with entry price
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  # Buy leg (pay premium)
  leg_buy <- make_leg(opt, quantity = 5, direction = "buy")
  expect_equal(leg.cash_flow(leg_buy), -5 * 10 * 100)  # -5,000
  
  # Sell leg (receive premium)
  leg_sell <- make_leg(opt, quantity = 3, direction = "sell")
  expect_equal(leg.cash_flow(leg_sell), 3 * 10 * 100)  # 3,000
  
  # Underlying with entry price
  stk <- make_underlying(symbol = "SPY", entry_price = 450)
  leg_stk <- make_leg(stk, quantity = 100, direction = "buy")
  expect_equal(leg.cash_flow(leg_stk), -100 * 450)  # -45,000
})

test_that("make_option prevents missing entry data", {
  # Should error when neither entry_price nor entry_iv provided
  expect_error(
    make_option(
      right = "c",
      strike = 100,
      expiry = as.Date("2026-12-31"),
      underlying_at_entry = 100
    ),
    "Either entry_price or entry_iv must be provided"
  )
})

test_that("leg.cash_flow works with instruments that have entry_price (always true)", {
  # All valid options have entry_price (either provided or calculated)
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  # Verify entry_price was auto-calculated
  expect_false(is.null(opt$entry_price))
  
  leg <- make_leg(opt, quantity = 5, direction = "buy")
  
  # Should work without warnings
  expect_silent(cf <- leg.cash_flow(leg))
  expect_true(cf < 0)
})

test_that("leg.cash_flow respects commission setting", {
  # Save original setting
  old_use_comm <- get_use_commission()
  on.exit(set_use_commission(old_use_comm))
  
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    commission = 1.50,
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  # Without commission
  set_use_commission(FALSE)
  leg <- make_leg(opt, quantity = 5, direction = "buy")
  expect_equal(leg.cash_flow(leg), -5 * 10 * 100)
  
  # With commission
  set_use_commission(TRUE)
  # Buy: commission is additional cost (negative)
  expect_equal(leg.cash_flow(leg), -5 * 10 * 100 - 1.50)
  
  # Sell: commission reduces proceeds
  leg_sell <- make_leg(opt, quantity = 5, direction = "sell")
  expect_equal(leg.cash_flow(leg_sell), 5 * 10 * 100 - 1.50)
})

test_that("leg getters work correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    multiplier = 100,
    commission = 1.50,
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg <- make_leg(opt, quantity = 5, direction = "buy")
  
  expect_equal(leg.get_instrument(leg), opt)
  expect_equal(leg.get_quantity(leg), 5)
  expect_equal(leg.get_direction(leg), "buy")
  expect_equal(leg.get_multiplier(leg), 100)
})

test_that("print.leg works without errors", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg_buy <- make_leg(opt, quantity = 5, direction = "buy")
  expect_output(print(leg_buy), "\\+5 x 100 contracts")
  expect_output(print(leg_buy), "Instrument: Dec31C100@10.00")
  
  leg_sell <- make_leg(opt, quantity = 3, direction = "sell")
  expect_output(print(leg_sell), "-3 x 100 contracts")
})