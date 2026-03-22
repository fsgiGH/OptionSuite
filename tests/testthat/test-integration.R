# tests/testthat/test-integration.R
# Integration tests combining legs, strategies, and instruments

test_that("Strategy can be built from instruments with real market data", {
  # Use the same SPX data from v0.1.1 tests
  spx_spot <- 6632.19
  entry_date <- as.Date("2026-03-16")
  
  # Create an OTM call option
  call_otm <- make_option(
    right = "c",
    strike = 6900,
    expiry = as.Date("2026-04-16"),
    style = "e",
    entry_price = 60.70,
    date_of_entry = entry_date,
    underlying_at_entry = spx_spot,
    dividend_yield_at_entry = 0
  )
  
  # Create a leg (buy 2 contracts)
  leg1 <- make_leg(call_otm, quantity = 2, direction = "buy")
  
  # Single-leg strategy
  strat <- make_strategy(list(leg1), "SPX Call")
  
  # Cash flow should be -2 * 60.70 * 100 = -12,140
  expect_equal(strategy.cash_flow(strat), -2 * 60.70 * 100)
  
  # Value at entry (should match option value)
  val <- strategy.value(strat, underlying = spx_spot, date = entry_date)
  expect_equal(val, 2 * 100 * instrument.value(call_otm), tolerance = 0.01)
})

test_that("Multi-leg strategy with different expiries calculates correctly", {
  # Create two options with different expiries
  opt1 <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-06-30"),
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  opt2 <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-09-30"),
    entry_price = 15,
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  leg1 <- make_leg(opt1, quantity = 1, direction = "sell")
  leg2 <- make_leg(opt2, quantity = 1, direction = "buy")
  
  strat <- make_strategy(list(leg1, leg2), "Calendar Spread")
  
  # Cash flow: +1,000 (sell) - 1,500 (buy) = -500
  expect_equal(strategy.cash_flow(strat), -500)
  
  # Value at June 30 (near-term expiration)
  expiry_near <- as.Date("2026-06-30")
  val <- strategy.value(strat, underlying = 105, date = expiry_near)
  
  # Near option expired (intrinsic 5) but we're short → -5 * 100 = -500
  # Far option still has time value (should be > intrinsic 5)
  # Total should be > 0 because far option value > near intrinsic
  expect_true(val > 0)
})