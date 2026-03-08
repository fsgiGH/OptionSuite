# valuation.R
# Core pricing functions for European and American options.

require(RQuantLib)

# ============================================================================
# European options (RQuantLib) with TTM threshold convention
# ============================================================================

#' Price a European option and return full Greeks
#' @param type "c" for call or "p" for put
#' @param S Spot price
#' @param K Strike price
#' @param r Risk‑free rate (if NULL, uses global)
#' @param q Dividend yield
#' @param T Time to maturity (years)
#' @param sigma Volatility
#' @param use_min_ttm Logical; if TRUE and T < min_reliable, use min_reliable for Greeks
#' @return List with components: value, delta, gamma, vega, theta, rho, dividendRho
price_european <- function(type = c("c", "p"),
                           S, K,
                           r = NULL, q,
                           T,
                           sigma,
                           use_min_ttm = FALSE) {
  type <- match.arg(type)
  if (is.null(r)) r <- get_global_r()
  min_ttm <- get_min_reliable_ttm()
  
  # --- Exact value at or below threshold ---
  if (T <= min_ttm) {
    intrinsic <- if (type == "c") max(S - K, 0) else max(K - S, 0)
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
      gr <- price_european(type, S, K, r, q, min_ttm, sigma, use_min_ttm = FALSE)
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
    volatility = sigma
  )
  
  list(
    value = opt$value,
    delta = opt$delta,
    gamma = opt$gamma,
    vega  = opt$vega,
    theta = opt$theta,
    rho   = opt$rho,
    dividendRho = opt$dividendRho
  )
}

# ============================================================================
# American options – Bjerksund–Stensland 2002
# ============================================================================

# ---- Helper functions ----

Phi <- function(x) pnorm(x)

bjerksund_params <- function(S, K, r, q, T, sigma) {
  b <- r - q
  if (T <= 0) return(list(beta = NA, alpha = NA, X1 = NA, X2 = NA))
  
  beta <- (0.5 - b/sigma^2) + sqrt((b/sigma^2 - 0.5)^2 + 2*r/sigma^2)
  # Critical price X_inf (perpetual option)
  X_inf <- if (b >= r) K else K * beta / (beta - 1)
  # Approximate X1, X2 (simplified; see Haug 2007 for details)
  h <- -(b*T + 2*sigma*sqrt(T)) * X_inf / (X_inf - K)
  X1 <- K + (X_inf - K) * (1 - exp(h))
  X2 <- X_inf * (1 + (1 - (X_inf/K)^(-beta)) / beta)
  alpha <- (X2 - K) * X2^(-beta)
  
  list(beta = beta, alpha = alpha, X1 = X1, X2 = X2, X_inf = X_inf)
}

#' Bjerksund–Stensland 2002 American call
bjerksund_call <- function(S, K, r, q, T, sigma) {
  if (T <= 0) return(max(S - K, 0))
  b <- r - q
  p <- bjerksund_params(S, K, r, q, T, sigma)
  
  if (S >= p$X2) return(S - K)  # immediate exercise
  
  d1 <- (log(S/p$X1) + (b + 0.5*sigma^2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  
  A1 <- p$alpha * S^p$beta
  A2 <- -p$alpha * Phi(-d1) * (S^p$beta) * exp((b - r)*T)
  A3 <- Phi(d2) * S * exp((b - r)*T)
  A4 <- -Phi(d2 - sigma*sqrt(T)) * K * exp(-r*T)
  A5 <- -p$alpha * (p$X1 - K) * (S/p$X1)^p$beta * Phi(d1) * exp((b - r)*T)
  
  price <- A1 + A2 + A3 + A4 + A5
  max(price, S - K)  # ensure not less than intrinsic
}

#' Bjerksund–Stensland 2002 American put (via put‑call symmetry)
bjerksund_put <- function(S, K, r, q, T, sigma) {
  if (T <= 0) return(max(K - S, 0))
  # Transform parameters
  call <- bjerksund_call(K, S, q, r, T, sigma)
  put <- K * exp(-r*T) - S * exp(-q*T) + call
  max(put, K - S)
}

# ---- Greeks via finite differences ----

#' Finite‑difference helpers
fd_greek <- function(f, x, h = 1e-4, ...) {
  (f(x * (1 + h), ...) - f(x * (1 - h), ...)) / (2 * x * h)
}

fd_gamma <- function(f, x, h = 1e-4, ...) {
  f_up <- f(x * (1 + h), ...)
  f_mid <- f(x, ...)
  f_down <- f(x * (1 - h), ...)
  (f_up - 2*f_mid + f_down) / (x * h)^2
}

fd_vega <- function(f, sigma, h = 1e-4, ...) {
  (f(sigma = sigma * (1 + h), ...) - f(sigma = sigma * (1 - h), ...)) / (2 * sigma * h)
}

fd_theta <- function(f, T, h = 1/365, ...) {
  if (T <= h) return(NA)
  (f(T = T - h, ...) - f(T = T, ...)) / h
}

fd_rho <- function(f, r, h = 1e-4, ...) {
  (f(r = r * (1 + h), ...) - f(r = r * (1 - h), ...)) / (2 * r * h)
}

#' Price an American option and return full Greeks
#' @param type "c" for call or "p" for put
#' @param S Spot
#' @param K Strike
#' @param r Risk‑free rate (if NULL, uses global)
#' @param q Dividend yield
#' @param T Time to maturity
#' @param sigma Volatility
#' @param use_min_ttm Logical; if TRUE and T < min_reliable, use min_reliable for Greeks
#' @return List with value, delta, gamma, vega, theta, rho
price_american <- function(type = c("c", "p"),
                           S, K,
                           r = NULL, q,
                           T,
                           sigma,
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
      gr <- price_american(type, S, K, r, q, min_ttm, sigma, use_min_ttm = FALSE)
      gr$value <- intrinsic
      return(gr)
    }
  }
  
  # --- Normal case: T > min_ttm ---
  val_fun <- if (type == "c") bjerksund_call else bjerksund_put
  
  # Base price
  value <- val_fun(S, K, r, q, T, sigma)
  
  # Greeks
  delta <- fd_greek(function(S, ...) val_fun(S, ...), S, h = 1e-4, K = K, r = r, q = q, T = T, sigma = sigma)
  gamma <- fd_gamma(function(S, ...) val_fun(S, ...), S, h = 1e-4, K = K, r = r, q = q, T = T, sigma = sigma)
  vega  <- fd_vega(function(sigma, ...) val_fun(S, K, r, q, T, sigma), sigma, h = 1e-4)
  theta <- fd_theta(function(T, ...) val_fun(S, K, r, q, T, sigma), T, h = 1/365)
  rho   <- fd_rho(function(r, ...) val_fun(S, K, r, q, T, sigma), r, h = 1e-4)
  
  list(value = value, delta = delta, gamma = gamma, vega = vega, theta = theta, rho = rho)
}

# ============================================================================
# Unified dispatcher
# ============================================================================

#' Price an option (European or American) and return full Greeks
#' 
#' Valuation is provided by applying the Black-Scholes model for European Options through the QuantLib public library
#' or the Bjerksund–Stensland (2002) closed model for American options through an internally-developed implementation.
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
#' @param sigma Volatility
#' @param ... Additional arguments passed to specific methods
#' 
#' @return List with value and Greeks
#' 
#' @examples
#' #As of March 8th 2026, BMY Apr17Call60 is quoted on TWS 2.36x2.51, while BMY quotes 60.22
#' #ATM Implied volatility is reported as 28.8%. The very same data are confirmed on ToS, besides
#' #BMY being quoted 60.29 and the bid-ask values for the option being 2.31x2.51
#' #Dividend yield is indicated as 4.2% and interest rate as 4.5%
#' 
#' get_day_count_convention()
#' expiry <- as.Date("2026-04-17")
#' valuation_date <- as.Date("2026-03-08")
#' ttm <- date_to_ttm(expiry, valuation_date)
#' price_option(style = "a",
#'              type = "c",
#'              60.22,
#'              60,
#'              r = 0.045,
#'              q = 0.042,
#'              T = ttm,
#'              0.288)
#' 
#' @export
price_option <- function(style = c("e", "a"),
                         type = c("c", "p"),
                         S, K,
                         r = NULL, 
                         q = 0,
                         T,
                         sigma,
                         ...) {
  if(get_day_count_convention() == "trading/252") {
    # Convert from trading-day annualization to calendar-day annualization
    # Model expects σ_calendar where daily = σ_calendar/√365
    # Input σ_trading where daily = σ_trading/√252
    # Therefore σ_calendar = σ_trading * √(365/252)
    sigma <- sigma * sqrt(365/252)
  }
  style <- match.arg(style)
  if (style == "e") {
    price_european(type, S, K, r, q, T, sigma, ...)
  } else {
    price_american(type, S, K, r, q, T, sigma, ...)
  }
}