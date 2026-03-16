test_that("instrument.value works for options with defaults", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100,
    date_of_entry = as.Date("2026-01-01")
  )
  
  # Value at entry should approximate entry price
  expect_equal(instrument.value(opt), 10, tolerance = 0.1)
  
  # Value with different spot
  val_up <- instrument.value(opt, underlying = 110)
  expect_true(val_up > 10)
  
  val_down <- instrument.value(opt, underlying = 90)
  expect_true(val_down < 10)
})

test_that("instrument.value works for options with overrides", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  # Override volatility
  val_high_iv <- instrument.value(opt, underlying = 100, ttm = 1, iv = 0.3)
  val_low_iv <- instrument.value(opt, underlying = 100, ttm = 1, iv = 0.1)
  expect_true(val_high_iv > val_low_iv)
  
  # Override rate
  val_high_r <- instrument.value(opt, underlying = 100, ttm = 1, r = 0.1)
  val_low_r <- instrument.value(opt, underlying = 100, ttm = 1, r = 0.01)
  expect_true(val_high_r > val_low_r)  # Calls increase with rate
})

test_that("instrument.value works for underlyings", {
  stk <- make_underlying(
    symbol = "SPY",
    entry_price = 450,
    date_of_entry = as.Date("2026-01-01")
  )
  
  # Value at entry
  expect_equal(instrument.value(stk), 450)
  
  # Value with different price
  expect_equal(instrument.value(stk, underlying = 460), 460)
  
  # Underlying ignores other parameters
  val <- instrument.value(stk, underlying = 455, ttm = 1, iv = 0.3, r = 0.1)
  expect_equal(val, 455)
})

test_that("instrument.delta works correctly", {
  # ATM call option
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  delta <- instrument.delta(opt, underlying = 100, ttm = 1)
  expect_true(delta > 0.5 && delta < 0.7)
  
  # Underlying delta always 1
  stk <- make_underlying("SPY")
  expect_equal(instrument.delta(stk), 1)
})

test_that("instrument.gamma works correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  gamma <- instrument.gamma(opt, underlying = 100, ttm = 1)
  expect_true(gamma > 0.01 && gamma < 0.02)
  
  # Underlying gamma always 0
  stk <- make_underlying("SPY")
  expect_equal(instrument.gamma(stk), 0)
})

test_that("instrument.vega works correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  vega <- instrument.vega(opt, underlying = 100, ttm = 1)
  expect_true(vega > 0.3 && vega < 0.4)  # Per 1% IV change
  
  # Underlying vega always 0
  stk <- make_underlying("SPY")
  expect_equal(instrument.vega(stk), 0)
})

test_that("instrument.theta works correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  theta <- instrument.theta(opt, underlying = 100, ttm = 1)
  expect_true(theta < 0 && theta > -0.02)  # Daily theta
  
  # Underlying theta always 0
  stk <- make_underlying("SPY")
  expect_equal(instrument.theta(stk), 0)
})

test_that("instrument.rho works correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  rho <- instrument.rho(opt, underlying = 100, ttm = 1)
  expect_true(rho > 0.5 && rho < 0.6)  # Per 1% rate change
  
  # Underlying rho always 0
  stk <- make_underlying("SPY")
  expect_equal(instrument.rho(stk), 0)
})

test_that("instrument.theor_price works correctly", {
  # Option with both price and IV
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_price = 10,
    entry_iv = 0.25,  # Different from what price would imply
    underlying_at_entry = 100
  )
  
  theor <- instrument.theor_price(opt, underlying = 100, ttm = 1)
  expect_true(abs(theor - 10) > 0.1)  # Should differ from entry price
  
  # Underlying theor_price equals value
  stk <- make_underlying("SPY", entry_price = 450)
  expect_equal(instrument.theor_price(stk), 450)
})

test_that("instrument.intrinsic works correctly", {
  # ITM call
  opt_call <- make_option(
    right = "c", 
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_iv = 0.2,
    underlying_at_entry = 100)
  expect_equal(instrument.intrinsic(opt_call, underlying = 110), 10)
  expect_equal(instrument.intrinsic(opt_call, underlying = 90), 0)
  
  # ITM put
  opt_put <- make_option(
    right = "p", 
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_iv = 0.2,
    underlying_at_entry = 100)
  expect_equal(instrument.intrinsic(opt_put, underlying = 90), 10)
  expect_equal(instrument.intrinsic(opt_put, underlying = 110), 0)
  
  # Underlying intrinsic = price
  stk <- make_underlying("SPY")
  expect_equal(instrument.intrinsic(stk, underlying = 450), 450)
})

test_that("instrument.extrinsic works correctly", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_iv = 0.2,
    underlying_at_entry = 100
  )
  
  extrinsic <- instrument.extrinsic(opt, underlying = 100, ttm = 1)
  expect_true(extrinsic > 0)  # ATM option has time value
  
  # Underlying extrinsic always 0
  stk <- make_underlying("SPY")
  expect_equal(instrument.extrinsic(stk), 0)
})

test_that("instrument.analytics returns complete data frame", {
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    style = "e",
    entry_price = 10,
    entry_iv = 0.2,
    underlying_at_entry = 100,
    dividend_yield_at_entry = 0.02
  )
  
  df <- instrument.analytics(opt, underlying = 105, ttm = 0.5)
  
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1)
  expect_true("delta" %in% names(df))
  expect_true("gamma" %in% names(df))
  expect_true("vega" %in% names(df))
  expect_true("theta" %in% names(df))
  expect_true("rho" %in% names(df))
  expect_true("intrinsic" %in% names(df))
  expect_true("extrinsic" %in% names(df))
  expect_true("mispricing" %in% names(df))
  
  # Underlying analytics
  stk <- make_underlying("SPY", entry_price = 450)
  df_stk <- instrument.analytics(stk, underlying = 460)
  expect_equal(df_stk$delta, 1)
  expect_equal(df_stk$gamma, 0)
})

test_that("instrument methods error gracefully with missing data", {
  # Option without underlying_at_entry (should error)
  expect_error(
    make_option(
      right = "c",
      strike = 100,
      expiry = as.Date("2026-12-31"),
      entry_iv = 0.2
      # No underlying_at_entry
    ),
    "underlying_at_entry is required when entry_iv is provided without entry_price"
  )
  
  # Option without iv (works because iv calculated from price)
  opt <- make_option(
    right = "c",
    strike = 100,
    expiry = as.Date("2026-12-31"),
    entry_price = 10,
    underlying_at_entry = 100
  )
  expect_s3_class(opt, "option")
  
  # Underlying without entry_price (should error when value called)
  stk <- make_underlying("SPY")
  expect_error(
    instrument.value(stk),
    "No price provided or stored"
  )
})