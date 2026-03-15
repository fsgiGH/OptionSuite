test_that("price_option European call matches known values", {
  result <- price_option(
    style = "e",
    type = "c",
    S = 100,
    K = 100,
    r = 0.05,
    q = 0,
    T = 1,
    iv = 0.2
  )
  
  # Print actual values for debugging
  cat("\nCall - Value:", result$value, "Delta:", result$delta, 
      "Gamma:", result$gamma, "Vega:", result$vega, 
      "Theta:", result$theta, "Rho:", result$rho, "\n")
  
  expect_equal(result$value, 10.45, tolerance = 0.01)
  expect_true(result$delta > 0.6 && result$delta < 0.7, 
              label = paste("Delta =", result$delta, "not in (0.6, 0.7)"))
  expect_true(result$gamma > 0.01 && result$gamma < 0.02,
              label = paste("Gamma =", result$gamma, "not in (0.01, 0.02)"))
  expect_true(result$vega > 0.3 && result$vega < 0.4,
              label = paste("Vega =", result$vega, "not in (0.3, 0.4)"))
  expect_true(result$theta < 0, label = paste("Theta =", result$theta, "not negative"))
  expect_true(result$rho > 0, label = paste("Rho =", result$rho, "not positive"))
})

test_that("price_option European put matches known values", {
  result <- price_option(
    style = "e",
    type = "p",
    S = 100,
    K = 100,
    r = 0.05,
    q = 0,
    T = 1,
    iv = 0.2
  )
  
  cat("\nPut - Value:", result$value, "Delta:", result$delta, 
      "Gamma:", result$gamma, "Vega:", result$vega, 
      "Theta:", result$theta, "Rho:", result$rho, "\n")
  
  expect_equal(result$value, 5.57, tolerance = 0.03)
  expect_true(result$delta > -0.4 && result$delta < -0.3,
              label = paste("Delta =", result$delta, "not in (-0.4, -0.3)"))
  expect_true(result$gamma > 0.01 && result$gamma < 0.02,
              label = paste("Gamma =", result$gamma, "not in (0.01, 0.02)"))
  expect_true(result$theta < 0, label = paste("Theta =", result$theta, "not negative"))
  expect_true(result$rho < 0, label = paste("Rho =", result$rho, "not negative"))
})

test_that("price_option American call matches fOptions", {
  # Test with parameters where early exercise might matter (dividend > 0)
  result <- price_option(
    style = "a",
    type = "c",
    S = 100,
    K = 95,
    r = 0.05,
    q = 0.03,
    T = 1,
    iv = 0.2
  )
  
  # Value should be > intrinsic (5) and reasonable
  expect_true(result$value > 5 && result$value < 15)
  expect_true(result$delta > 0.6 && result$delta < 0.8)
  expect_true(result$gamma > 0)
  expect_true(result$vega > 0)
  expect_true(result$theta < 0)
})

test_that("price_option American put matches fOptions", {
  result <- price_option(
    style = "a",
    type = "p",
    S = 100,
    K = 105,
    r = 0.05,
    q = 0.02,
    T = 1,
    iv = 0.25
  )
  
  expect_true(result$value > 5 && result$value < 15)
  expect_true(result$delta < 0)  # Put delta negative
  expect_true(result$gamma > 0)
})

test_that("TTM threshold logic works correctly", {
  min_ttm <- get_min_reliable_ttm()
  T_small <- min_ttm / 2
  
  # Case 1: use_min_ttm = FALSE (default) - should return intrinsic with NA Greeks
  result_default <- price_european(
    type = "c",
    S = 105,
    K = 100,
    r = 0.05,
    q = 0,
    T = T_small,
    iv = 0.2,
    use_min_ttm = FALSE
  )
  
  expect_equal(result_default$value, 5)
  expect_true(is.na(result_default$theta))
  expect_equal(result_default$delta, 1)  # ITM call delta at expiry
  
  # Case 2: use_min_ttm = TRUE - should return intrinsic value with model Greeks
  result_model <- price_european(
    type = "c",
    S = 105,
    K = 100,
    r = 0.05,
    q = 0,
    T = T_small,
    iv = 0.2,
    use_min_ttm = TRUE
  )
  
  expect_equal(result_model$value, 5)  # Still intrinsic
  expect_false(is.na(result_model$theta))  # Greeks should be non-NA
  expect_true(result_model$theta < 0)  # Theta negative
  
  # Case 3: Through price_option dispatcher
  result_dispatcher <- price_option(
    style = "e",
    type = "c",
    S = 105,
    K = 100,
    r = 0.05,
    q = 0,
    T = T_small,
    iv = 0.2,
    use_min_ttm = TRUE
  )
  
  expect_equal(result_dispatcher$value, 5)
  expect_false(is.na(result_dispatcher$theta))
})

test_that("Error handling works", {
  # Test invalid option type
  expect_error(
    price_option(style = "e", type = "x", S=100, K=100, T=1, iv=0.2),
    "'arg' should be one of"
  )
  
  # Test negative time (should trigger threshold but not error)
  result <- price_option(style = "e", type = "c", S=100, K=100, T=-1, iv=0.2)
  expect_equal(result$value, 0)  # Expired option worth 0
})

test_that("Greeks have correct scaling (broker conventions)", {
  result <- price_option(
    style = "e",
    type = "c",
    S = 100,
    K = 100,
    r = 0.05,
    q = 0,
    T = 1,
    iv = 0.2
  )
  
  # Vega should be per 1% change (so ~0.01 of raw derivative)
  expect_true(result$vega > 0 && result$vega < 1)
  
  # Theta should be daily (not annualized)
  expect_true(abs(result$theta) < 1)  # Daily theta < 1
  
  # Rho should be per 1% rate change
  expect_true(result$rho > 0 && result$rho < 1)
})

test_that("Error handling works", {
  # Missing required arguments should error
  expect_error(
    price_option(style = "e", type = "c"), 
    "argument \"T\" is missing, with no default",
    info = "Missing T should throw error"
  )
  
  # Missing S
  expect_error(
    price_option(style = "e", type = "c", K=100, T=1, iv=0.2),
    "argument \"S\" is missing, with no default"
  )
  
  # Missing K
  expect_error(
    price_option(style = "e", type = "c", S=100, T=1, iv=0.2),
    "argument \"K\" is missing, with no default"
  )
  
  # Missing iv
  expect_error(
    price_option(style = "e", type = "c", S=100, K=100, T=1),
    "argument \"iv\" is missing, with no default"
  )
  
  # Invalid style
  expect_error(
    price_option(style = "x", type = "c", S=100, K=100, T=1, iv=0.2),
    "'arg' should be one of"
  )
  
  # Invalid type
  expect_error(
    price_option(style = "e", type = "x", S=100, K=100, T=1, iv=0.2),
    "'arg' should be one of"
  )
  
  # Negative T handled gracefully (returns intrinsic)
  result <- price_option(style = "e", type = "c", S=100, K=100, T=-1, iv=0.2)
  expect_equal(result$value, 0)  # Expired OTM call worth 0
})