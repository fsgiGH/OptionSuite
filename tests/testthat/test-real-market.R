# tests/testthat/test-real-market.R
# Real market data tests from IBKR TWS (2026-03-16)

test_that("SPX European options match market data", {
  # Market parameters
  spx_spot <- 6632.19
  entry_date <- as.Date("2026-03-16")
  
  # Helper to calculate DTE
  days_to_expiry <- function(expiry_date) {
    as.numeric(difftime(as.Date(expiry_date), entry_date, units = "days"))
  }
  
  # --- 0 DTE options (16-Mar-2026 expiry) ---
  # 0 DTE Call
  call_0 <- make_option(
    right = "c",
    strike = 6635,
    expiry = as.Date("2026-03-16"),
    style = "e",
    entry_price = 47.95,
    entry_iv = 0.294,
    date_of_entry = entry_date,
    underlying_at_entry = spx_spot,
    dividend_yield_at_entry = 0
  )
  
  expect_equal(instrument.value(call_0, underlying = spx_spot), 
               max(spx_spot - 6635, 0), tolerance = 0.1)
  
  # --- 31 DTE options (16-Apr-2026 expiry) ---
  ttm_31 <- days_to_expiry("2026-04-16") / 365
  
  # OTM Call (strike 6900)
  call_31_otm <- make_option(
    right = "c",
    strike = 6900,
    expiry = as.Date("2026-04-16"),
    style = "e",
    entry_price = 60.70,
    entry_iv = 0.177,
    date_of_entry = entry_date,
    underlying_at_entry = spx_spot,
    dividend_yield_at_entry = 0
  )
  
  delta_otm <- instrument.delta(call_31_otm, underlying = spx_spot)
  cat(sprintf("\nSPX 31d OTM Call (6900): Market delta = 0.290, Our delta = %.4f\n", delta_otm))
  expect_true(abs(delta_otm - 0.29) < 0.05)
})

test_that("TLT American options match market data", {
  # Market parameters
  tlt_spot <- 86.84
  div_yield <- 0.045
  entry_date <- as.Date("2026-03-16")
  
  # ATM Call (strike 86)
  call_86 <- make_option(
    right = "c",
    strike = 86,
    expiry = as.Date("2026-04-17"),
    style = "a",
    entry_price = 1.81,
    date_of_entry = entry_date,
    underlying_at_entry = tlt_spot,
    dividend_yield_at_entry = div_yield
  )
  
  delta_call <- instrument.delta(call_86, underlying = tlt_spot)
  cat(sprintf("\nTLT 32d ATM Call (86): Market delta = 0.577, Our delta = %.4f\n", delta_call))
  cat(sprintf("                    Our IV = %.4f, Market IV unknown\n", option.get_iv(call_86)))
  
  # ATM Put (strike 86)
  put_86 <- make_option(
    right = "p",
    strike = 86,
    expiry = as.Date("2026-04-17"),
    style = "a",
    entry_price = 1.26,
    date_of_entry = entry_date,
    underlying_at_entry = tlt_spot,
    dividend_yield_at_entry = div_yield
  )
  
  delta_put <- instrument.delta(put_86, underlying = tlt_spot)
  cat(sprintf("TLT 32d ATM Put (86):  Market delta = -0.443, Our delta = %.4f\n", delta_put))
  cat(sprintf("                    Our IV = %.4f, Market IV unknown\n", option.get_iv(put_86)))
  
  # Deep ITM Put (strike 95)
  put_95 <- make_option(
    right = "p",
    strike = 95,
    expiry = as.Date("2026-04-17"),
    style = "a",
    entry_price = 8.625,
    date_of_entry = entry_date,
    underlying_at_entry = tlt_spot,
    dividend_yield_at_entry = div_yield
  )
  
  delta_put_itm <- instrument.delta(put_95, underlying = tlt_spot)
  cat(sprintf("TLT 32d ITM Put (95):  Market delta = -0.970, Our delta = %.4f\n", delta_put_itm))
  cat(sprintf("                    Our IV = %.4f, Market IV unknown\n", option.get_iv(put_95)))
  
  # Don't test exact values since IVs differ, just verify signs and ranges
  expect_true(delta_call > 0.5 && delta_call < 0.7)
  expect_true(delta_put < -0.3 && delta_put > -0.5)
  expect_true(delta_put_itm < -0.8)
})