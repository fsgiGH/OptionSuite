# valuation.R
# Core pricing functions for European and American options.

require(RQuantLib)
require(fOptions)

# ============================================================================
# European options (RQuantLib) with TTM threshold convention
# ============================================================================

#' Price a European option and return full Greeks
#' @param type "c" for call or "p" for put
#' @param S Spot price
#' @param K Strike price
#' @param r Risk free rate (if NULL, uses global)
#' @param q Dividend yield
#' @param T Time to maturity (years)
#' @param iv Implied volatility
#' @param use_min_ttm Logical; if TRUE and T < min_reliable, use min_reliable for Greeks
#' @return List with components: value, delta, gamma, vega, theta, rho, dividendRho
#' @keywords internal
price_european <- function(type = c("c", "p"),
                           S, K,
                           r = NULL, q,
                           T,
                           iv,
                           use_min_ttm = FALSE) {
  type <- match.arg(type)
  if (is.null(r)) r <- get_global_r()
  min_ttm <- get_min_reliable_ttm()
  
  # --- Exact value at or below threshold ---
  if (T <= min_ttm) {
    intrinsic <- if (type == "c") max(S - K, 0) else max(K - S, 0)
    
    # BASE CASE: When T == min_ttm, compute full price (not intrinsic)
    if (T == min_ttm) {
      rquantlib_type <- ifelse(type == "c", "call", "put")
      opt <- RQuantLib::EuropeanOption(
        type      = rquantlib_type,
        underlying = S,
        strike    = K,
        dividendYield = q,
        riskFreeRate = r,
        maturity  = T,
        volatility = iv
      )
      return(list(
        value = opt$value,
        delta = opt$delta,
        gamma = opt$gamma,
        vega  = opt$vega * 0.01,
        theta = opt$theta / 365,
        rho   = opt$rho * 0.01,
        dividendRho = if (!is.null(opt$dividendRho)) opt$dividendRho * 0.01 else NULL
      ))
    }
    
    # For T < min_ttm
    if (T == 0 || !use_min_ttm) {
      # Greeks at exact expiry are theoretical (step function)
      delta <- if (type == "c") {
        if (S > K) 1 else if (S < K) 0 else NA
      } else {
        if (S < K) -1 else if (S > K) 0 else NA
      }
      return(list(
        value = intrinsic,
        delta = delta,
        gamma = 0,
        vega = 0,
        theta = NA,
        rho = 0,
        dividendRho = 0
      ))
    } else {
      # Value = exact, Greeks from min_reliable_ttm
      gr <- price_european(type, S, K, r, q, min_ttm, iv, use_min_ttm = TRUE)
      gr$value <- intrinsic
      return(gr)
    }
  }
  
  rquantlib_type <- ifelse(type == "c", "call", "put")
  
  # --- Normal case: T > min_ttm ---
  opt <- RQuantLib::EuropeanOption(
    type      = rquantlib_type,
    underlying = S,
    strike    = K,
    dividendYield = q,
    riskFreeRate = r,
    maturity  = T,
    volatility = iv
  )
  
  # Apply broker-convention scaling
  list(
    value = opt$value,
    delta = opt$delta,
    gamma = opt$gamma,
    vega  = opt$vega * 0.01,
    theta = opt$theta / 365,
    rho   = opt$rho * 0.01,
    dividendRho = if (!is.null(opt$dividendRho)) opt$dividendRho * 0.01 else NULL
  )
}

# ============================================================================
# American options – using fOptions for pricing, finite differences for Greeks
# ============================================================================

# ---- Helper functions (finite differences) ----

Phi <- function(x) pnorm(x)

#' Finite difference helpers for Greeks calculation
#' 
#' @title Finite difference helpers
#' @description
#' These functions are internal helpers for calculating Greeks via finite differences.
#' They are not intended for direct use by end users.
#' 
#' @param f A function to differentiate
#' @param x Point at which to evaluate the derivative
#' @param h Step size for finite difference
#' @param iv Implied volatility for vega calculation
#' @param T Time to maturity for theta calculation
#' @param r Risk-free rate for rho calculation
#' @param ... Additional arguments passed to f
#' 
#' @keywords internal
#' @name fd_helpers
NULL

#' @rdname fd_helpers
#' keywords internal
fd_greek <- function(f, x, h = 1e-4, ...) {
  (f(x * (1 + h), ...) - f(x * (1 - h), ...)) / (2 * x * h)
}


#' @rdname fd_helpers
#' keywords internal
fd_gamma <- function(f, x, h = 1e-4, ...) {
  f_up <- f(x * (1 + h), ...)
  f_mid <- f(x, ...)
  f_down <- f(x * (1 - h), ...)
  (f_up - 2*f_mid + f_down) / (x * h)^2
}


#' @rdname fd_helpers
#' keywords internal
fd_vega <- function(f, iv, h = 1e-4, ...) {
  # Raw derivative: change for 1.0 change in iv
  raw_deriv <- (f(iv = iv * (1 + h), ...) - f(iv = iv * (1 - h), ...)) / (2 * iv * h)
  # Scale to 1% (0.01) change (broker convention)
  raw_deriv * 0.01
}

#' @rdname fd_helpers
#' keywords internal
fd_theta <- function(f, T, h = 1/365, ...) {
  if (T <= h) return(NA)
  # Daily theta (broker convention)
  (f(T = T - h, ...) - f(T = T, ...)) / 1
}


#' @rdname fd_helpers
#' keywords internal
fd_rho <- function(f, r, h = 1e-4, ...) {
  # Raw derivative: change for 1.0 change in r
  raw_deriv <- (f(r = r * (1 + h), ...) - f(r = r * (1 - h), ...)) / (2 * r * h)
  # Scale to 1% (0.01) change (broker convention)
  raw_deriv * 0.01
}

#' Price an American option and return full Greeks
#' @param type "c" for call or "p" for put
#' @param S Spot
#' @param K Strike
#' @param r Risk free rate (if NULL, uses global)
#' @param q Dividend yield
#' @param T Time to maturity
#' @param iv Implied volatility
#' @param use_min_ttm Logical; if TRUE and T < min_reliable, use min_reliable for Greeks
#' @return List with value, delta, gamma, vega, theta, rho
#' @keywords internal
price_american <- function(type = c("c", "p"),
                           S, K,
                           r = NULL, q,
                           T,
                           iv,
                           use_min_ttm = FALSE) {
  type <- match.arg(type)
  if (is.null(r)) r <- get_global_r()
  min_ttm <- get_min_reliable_ttm()
  
  # --- Exact value at or below threshold ---
  if (T <= min_ttm) {
    intrinsic <- if (type == "c") max(S - K, 0) else max(K - S, 0)
    if (T == 0 || !use_min_ttm) {
      # Greeks at exact expiry (theoretical)
      delta <- if (type == "c") {
        if (S > K) 1 else if (S < K) 0 else NA
      } else {
        if (S < K) -1 else if (S > K) 0 else NA
      }
      return(list(
        value = intrinsic,
        delta = delta,
        gamma = 0,
        vega = 0,
        theta = NA,
        rho = 0
      ))
    } else {
      # Value = exact, Greeks from min_reliable_ttm
      gr <- price_american(type, S, K, r, q, min_ttm, iv, use_min_ttm = FALSE)
      gr$value <- intrinsic
      return(gr)
    }
  }
  
  # --- Normal case: T > min_ttm ---
  b <- r - q  # cost of carry
  type_flag <- if (type == "c") "c" else "p"
  
  # Use fOptions for pricing (reliable implementation)
  # Note: fOptions expects parameter 'sigma', we pass our 'iv'
  value <- fOptions::BSAmericanApproxOption(
    TypeFlag = type_flag,
    S = S,
    X = K,
    Time = T,
    r = r,
    b = b,
    sigma = iv
  )@price
  
  # Greeks via finite differences
  # Wrapper function for fOptions call
  val_fun <- function(S_val = S, iv_val = iv, r_val = r, T_val = T) {
    fOptions::BSAmericanApproxOption(
      TypeFlag = type_flag,
      S = S_val,
      X = K,
      Time = T_val,
      r = r_val,
      b = r_val - q,
      sigma = iv_val
    )@price
  }
  
  # Calculate Greeks using finite differences
  delta <- fd_greek(
    function(x, ...) val_fun(S_val = x, ...), 
    S, h = 1e-4, iv_val = iv, r_val = r, T_val = T
  )
  
  gamma <- fd_gamma(
    function(x, ...) val_fun(S_val = x, ...), 
    S, h = 1e-4, iv_val = iv, r_val = r, T_val = T
  )
  
  vega <- fd_vega(
    function(iv_arg, ...) val_fun(iv_val = iv_arg, ...), 
    iv, h = 1e-4, S_val = S, r_val = r, T_val = T
  )
  
  theta <- fd_theta(
    function(T_val, ...) val_fun(T_val = T_val, ...), 
    T, h = 1/365, S_val = S, iv_val = iv, r_val = r
  )
  
  rho <- fd_rho(
    function(r_val, ...) val_fun(r_val = r_val, ...), 
    r, h = 1e-4, S_val = S, iv_val = iv, T_val = T
  )
  
  list(
    value = value,
    delta = delta,
    gamma = gamma,
    vega = vega,
    theta = theta,
    rho = rho
  )
}

# ============================================================================
# Unified dispatcher
# ============================================================================

#' Price an option (European or American) and return full Greeks
#' 
#' Valuation is provided by applying the Black-Scholes model for European Options through the QuantLib public library
#' or the Bjerksund–Stensland (2002) closed model for American options through the fOptions implementation.
#' Given the usual input parameters listed below, the returned object is a list containing option value at the given 
#' time to maturity and volatility levels, plus all the first-order greeks: delta, vega, theta, gamma, and rho.
#' 
#' @param style "e" for European" or "a" for American
#' @param type "c" for call or "p" for put
#' @param S Spot
#' @param K Strike
#' @param r Rate (NULL uses global)
#' @param q Dividend yield (0 by default)
#' @param T Time to maturity
#' @param iv Implied volatility
#' @param ... Additional arguments passed to specific methods
#' 
#' @return List with value and Greeks
#' 
#' @examples
#' # As of March 8th 2026, BMY Apr17Call60 is quoted on TWS 2.36x2.51, while BMY quotes 60.22
#' # ATM Implied volatility is reported as 28.8%. The very same data are confirmed on ToS, besides
#' # BMY being quoted 60.29 and the bid-ask values for the option being 2.31x2.51
#' # Dividend yield is indicated as 4.2% and interest rate as 4.5%
#' 
#' get_day_count_convention()
#' expiry <- as.Date("2026-04-17")
#' valuation_date <- as.Date("2026-03-08")
#' ttm <- date_to_ttm(expiry, valuation_date)
#' price_option(style = "a",
#'              type = "c",
#'              S = 60.22,
#'              K = 60,
#'              r = 0.045,
#'              q = 0.042,
#'              T = ttm,
#'              iv = 0.288)
#'              
#' # As of March 8th 2026, SPX May14Put6740 is quoted on TWS 228.10x234.10, 
#' # while S&P 500 quotes 6738.15 ATM implied volatility is reported 
#' # as 20.5% while VIX is 29.49. ToS reports IV of 22.40%.
#' # These options are European style with no dividends
#' 
#' expiry <- as.Date("2026-05-14")
#' valuation_date <- as.Date("2026-03-08")
#' ttm <- date_to_ttm(expiry, valuation_date)
#' price_option(style = "e",
#'              type = "p",
#'              S = 6738.15,
#'              K = 6740,
#'              r = 0.045,
#'              q = 0,
#'              T = ttm,
#'              iv = 0.224)
#' 
#' @export
price_option <- function(style = c("e", "a"),
                         type = c("c", "p"),
                         S, K,
                         r = NULL, 
                         q = 0,
                         T,
                         iv,
                         ...) {
  if(get_day_count_convention() == "trading/252") {
    # Convert from trading-day annualization to calendar-day annualization
    # Model expects σ_calendar where daily = σ_calendar/√365
    # Input σ_trading where daily = σ_trading/√252
    # Therefore σ_calendar = σ_trading * √(365/252)
    iv <- iv * sqrt(365/252)
  }
  style <- match.arg(style)
  if (style == "e") {
    price_european(type, S, K, r, q, T, iv, ...)
  } else {
    price_american(type, S, K, r, q, T, iv, ...)
  }
}
