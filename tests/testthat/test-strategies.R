# tests/testthat/test-strategies.R
# Tests for strategy class functionality

test_that("make_strategy validates inputs correctly", {
  # Create legs
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg1 <- make_leg(opt, quantity = 5, direction = "buy")
  leg2 <- make_leg(opt, quantity = 2, direction = "sell")
  
  # Valid strategy
  strat <- make_strategy(list(leg1, leg2), name = "Test Strategy")
  expect_s3_class(strat, "strategy")
  expect_equal(strat$name, "Test Strategy")
  expect_equal(strat$n_legs, 2)
  
  # Strategy without name gets default
  strat2 <- make_strategy(list(leg1))
  expect_equal(strat2$name, "Strategy")
  
  # Empty legs should error
  expect_error(make_strategy(list()), "at least one leg")
  
  # Non-leg objects should error
  expect_error(make_strategy(list(leg1, "not a leg")), "not a leg object")
})

test_that("strategy.cash_flow sums correctly", {
  opt1 <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  opt2 <- make_option(
    right = "p",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 8,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg_buy_call <- make_leg(opt1, quantity = 5, direction = "buy")   # -5,000
  leg_sell_put <- make_leg(opt2, quantity = 3, direction = "sell")  # +2,400
  
  strat <- make_strategy(list(leg_buy_call, leg_sell_put))
  expect_equal(strategy.cash_flow(strat), -5000 + 2400)  # -2,600
})

test_that("strategy.value handles single date correctly", {
  # Create options with different expiries
  opt_near <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-06-30"),
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  opt_far <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg_near <- make_leg(opt_near, quantity = 1, direction = "buy")
  leg_far <- make_leg(opt_far, quantity = 1, direction = "buy")
  
  strat <- make_strategy(list(leg_near, leg_far), "Calendar Spread")
  
  # Value at a mid-point date (Sep 30)
  valuation_date <- as.Date("2026-09-30")
  
  # Each leg calculates its own TTM
  ttm_near <- date_to_ttm(opt_near$expiry, valuation_date)  # Should be negative (expired)
  ttm_far <- date_to_ttm(opt_far$expiry, valuation_date)   # Should be positive
  
  # Near option is expired, should have value 0 (intrinsic)
  # Far option still has time value
  value <- strategy.value(strat, underlying = 105, date = valuation_date)
  expect_true(value > 0)  # At least the far option value
  
  # Test with character date input
  value_char <- strategy.value(strat, underlying = 105, date = "2026-09-30")
  expect_equal(value, value_char)
})

test_that("strategy.payoff equals value + cash flow", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg <- make_leg(opt, quantity = 5, direction = "buy")
  strat <- make_strategy(list(leg))
  
  date <- as.Date("2026-09-30")
  underlying <- 105
  
  value <- strategy.value(strat, underlying, date)
  cash_flow <- strategy.cash_flow(strat)
  payoff <- strategy.payoff(strat, underlying, date)
  
  expect_equal(payoff, value + cash_flow)
})

test_that("strategy with underlying and option (covered call) works", {
  # Create stock and call
  stk <- make_underlying(symbol = "SPY", entry_price = 450)
  call <- make_option(
    right = "c",
    strike = 460,
    expiry = as.Date("2026-12-31"),
    entry_price = 5,
    entry_iv = 0.2,
    underlying_at_entry = 450
  )
  
  leg_stk <- make_leg(stk, quantity = 100, direction = "buy")
  leg_call <- make_leg(call, quantity = 1, direction = "sell")
  
  covered_call <- make_strategy(list(leg_stk, leg_call), "Covered Call")
  
  # Cash flow: -45,000 + 500 = -44,500
  expect_equal(strategy.cash_flow(covered_call), -45000 + 500)
  
  # Value at expiration with SPY = 455
  # Stock: 455 * 100 = 45,500
  # Call: expired OTM = 0
  expiry <- as.Date("2026-12-31")
  value <- strategy.value(covered_call, underlying = 455, date = expiry)
  expect_equal(value, 455 * 100)  # Just stock value
  
  # Payoff at expiration = value + cash_flow = 45,500 - 44,500 = 1,000
  payoff <- strategy.payoff(covered_call, underlying = 455, date = expiry)
  expect_equal(payoff, 1000)
  
  # Value at expiration with SPY = 465 (ITM call)
  value <- strategy.value(covered_call, underlying = 465, date = expiry)
  # Stock: 465 * 100 = 46,500
  # Call: intrinsic 465 - 460 = 5, but we're short so -5 * 100 = -500
  # Total = 46,500 - 500 = 46,000
  expect_equal(value, 46500 - 500)
  
  # Payoff = 46,000 - 44,500 = 1,500
  payoff <- strategy.payoff(covered_call, underlying = 465, date = expiry)
  expect_equal(payoff, 1500)
})

test_that("strategy getters work correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg1 <- make_leg(opt, quantity = 5, direction = "buy")
  leg2 <- make_leg(opt, quantity = 3, direction = "sell")
  
  strat <- make_strategy(list(leg1, leg2), "Test")
  
  expect_equal(strategy.get_legs(strat), list(leg1, leg2))
  expect_equal(strategy.n_legs(strat), 2)
  expect_equal(strategy.get_name(strat), "Test")
  
  # Set new name - must assign the returned value
  strat <- strategy.set_name(strat, "New Name")
  expect_equal(strategy.get_name(strat), "New Name")
})

test_that("print.strategy works without errors", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg1 <- make_leg(opt, quantity = 5, direction = "buy")
  leg2 <- make_leg(opt, quantity = 3, direction = "sell")
  
  strat <- make_strategy(list(leg1, leg2), "Test Strategy")
  
  expect_output(print(strat), "Test Strategy")
  expect_output(print(strat), "Legs: 2")
  expect_output(print(strat), "Leg 1:")
  expect_output(print(strat), "Leg 2:")
})