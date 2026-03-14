#' Create an option instrument
#' 
#' Option is the basic S3 object of the Suite describing this derivative financial
#' instrument, provided with methods for extracting its value along time, up to expiration,
#' as well as greeks and othe useful data. make_option is the constructor for specifying
#' all the option features at entry.
#' 
#' Concerning entry_price (premium) and implied volatility, user, at construction, can
#' specify both, if known. In this case, the theor_price method will return the price of
#' the option as returned by the relevant theoretical pricing model, allowing an immediate
#' verification whether the indicated (quoted) price is fair, or it represent a contract
#' undervaluation or overvaluation.
#' 
#' In case the user provides only an entry price, the system automatically calculates the
#' corresponding implied volatility according to the relevant pricing model (Black-Scholes for
#' European style options or Bjerksund-Stensland for American options). In case implied volatility
#' is provided but no entry price, the entry price will be calculated according to the model.
#' For this being possible the user must provide the price of the underlying security in the appropriate
#' input argument, or the system produces a warning and an error.
#' 
#' @param right Character, "c" for call or "p" for put
#' @param strike Numeric, strike price
#' @param expiry Date or numeric (time to maturity in years)
#' @param style Character, "a" for American or "e" for European (default "a")
#' @param multiplier Numeric, number of underlying shares per contract (default 100)
#' @param commission Numeric, per‑contract commission (default 0)
#' @param entry_price Optional numeric, price paid/received at trade inception
#' @param entry_iv Optional numeric, implied volatility at inception (if known)
#' @param date_of_entry Date, trade inception date (default Sys.Date())
#' @param underlying_at_entry Numeric, underlying price at entry (required if entry_price provided without entry_iv)
#' @param dividend_yield_at_entry Numeric, dividend yield at entry (default 0)
#' 
#' @examples
#' may_put <- make_option(right = "p",
#'                        strike = 50,
#'                        expiry = as.Date("2026-05-15"),
#'                        style = "a",
#'                        commission = 1.00,
#'                        entry_price = 1.27,
#'                        underlying_at_entry = 51.7)
#' # Implied volatility was automatically calculated
#' (iv <- option.get_iv(may_put))  # Returns 0.2498553
#' 
#' # Use it in further calculations
#' option.value(may_put, underlying = 52, ttm = 0.1) * iv
#' 
#' print(may_put) 
#' 
#' @return Object of class "option"
#' @export
make_option <- function(right = c("c", "p"),
                        strike,
                        expiry,
                        style = c("a", "e"),
                        multiplier = 100,
                        commission = 0,
                        entry_price = NULL,
                        entry_iv = NULL,
                        date_of_entry = Sys.Date(),
                        underlying_at_entry = NULL,
                        dividend_yield_at_entry = 0) {
  right <- match.arg(right)
  style <- match.arg(style)
  
  # Basic validation
  stopifnot(is.numeric(strike), length(strike) == 1, strike > 0)
  stopifnot(is.numeric(multiplier), length(multiplier) == 1, multiplier > 0)
  stopifnot(is.numeric(commission), length(commission) == 1, commission >= 0)
  stopifnot(is.numeric(dividend_yield_at_entry), length(dividend_yield_at_entry) == 1, dividend_yield_at_entry >= 0)
  
  # Validate date_of_entry
  if (!inherits(date_of_entry, "Date")) {
    stop("date_of_entry must be a Date object")
  }
  
  # --- FLEXIBLE EXPIRY CONVERSION ---
  # Handle different input types for expiry
  if (is.numeric(expiry)) {
    # Numeric: treat as years from date_of_entry
    if (expiry < 0) stop("expiry (as year fraction) cannot be negative")
    days_to_expiry <- round(expiry * 365)
    expiry <- date_of_entry + days_to_expiry
    message(sprintf("Numeric expiry %.4f years converted to Date: %s", 
                    expiry, as.character(expiry)))
    
  } else if (is.character(expiry)) {
    # Character: try to parse with as.Date
    expiry_parsed <- as.Date(expiry)
    if (is.na(expiry_parsed)) {
      stop(sprintf("Cannot parse expiry string: '%s'. Use format 'YYYY-MM-DD'", expiry))
    }
    expiry <- expiry_parsed
    message(sprintf("Character expiry '%s' parsed as Date: %s", 
                    expiry, as.character(expiry)))
    
  } else if (inherits(expiry, "POSIXt")) {
    # POSIXct/POSIXlt: convert to Date
    expiry <- as.Date(expiry)
    message(sprintf("POSIX expiry converted to Date: %s", as.character(expiry)))
    
  } else if (!inherits(expiry, "Date")) {
    # Anything else is invalid
    stop("expiry must be numeric (years), a Date object, or a character string in 'YYYY-MM-DD' format")
  }
  # If it's already a Date, we keep it as is
  # --- END EXPIRY CONVERSION ---
  
  # Validate that at least one of entry_price or entry_iv is provided
  if (is.null(entry_price) && is.null(entry_iv)) {
    stop("Either entry_price or entry_iv must be provided")
  }
  
  # If entry_price provided without entry_iv, calculate IV
  if (!is.null(entry_price) && is.null(entry_iv)) {
    rquantlib_right <- ifelse(right == "c", "call", "put")
    if (is.null(underlying_at_entry)) {
      stop("underlying_at_entry is required when entry_price is provided without entry_iv")
    }
    
    # Calculate ttm at entry
    ttm_at_entry <- date_to_ttm(expiry, date_of_entry)
    
    # Calculate implied volatility based on option style
    if (style == "e") {
      # European option
      iv_result <- RQuantLib::EuropeanOptionImpliedVolatility(
        type = rquantlib_right,
        value = entry_price,
        underlying = underlying_at_entry,
        strike = strike,
        dividendYield = dividend_yield_at_entry,
        riskFreeRate = get_global_r(),
        maturity = ttm_at_entry,
        volatility = 0.2  # starting guess
      )
      entry_iv <- iv_result
    } else {
      # American option
      iv_result <- RQuantLib::AmericanOptionImpliedVolatility(
        type = rquantlib_right,
        value = entry_price,
        underlying = underlying_at_entry,
        strike = strike,
        dividendYield = dividend_yield_at_entry,
        riskFreeRate = get_global_r(),
        maturity = ttm_at_entry,
        volatility = 0.2  # starting guess
      )
      entry_iv <- iv_result
    }
  }
  
  # If entry_iv provided without entry_price, calculate price
  if (!is.null(entry_iv) && is.null(entry_price)) {
    if (is.null(underlying_at_entry)) {
      stop("underlying_at_entry is required when entry_iv is provided without entry_price")
    }
    
    # Calculate ttm at entry
    ttm_at_entry <- date_to_ttm(expiry, date_of_entry)
    
    # Calculate theoretical price
    price_result <- price_option(
      style = style,
      type = right,
      S = underlying_at_entry,
      K = strike,
      r = get_global_r(),
      q = dividend_yield_at_entry,
      T = ttm_at_entry,
      iv = entry_iv
    )
    entry_price <- price_result$value
  }
  
  # Both provided? We keep both 
  
  structure(
    list(
      right = right,
      strike = strike,
      expiry = expiry,
      style = style,
      multiplier = multiplier,
      commission = commission,
      entry_price = entry_price,
      iv = entry_iv,
      date_of_entry = date_of_entry,
      underlying_at_entry = underlying_at_entry,
      dividend_yield_at_entry = dividend_yield_at_entry
    ),
    class = "option"
  )
}

# ---- S3 methods ----

#' @export
print.option <- function(x, ...) {
  # Calculate months difference
  months_diff <- as.numeric(difftime(x$expiry, x$date_of_entry, units = "days")) / 30.44
  
  if (months_diff > 11) {
    # Show year for longer-dated options
    cat(sprintf("%s-%s%s%s@%.2f", 
                format(x$expiry, "%b%d"),
                format(x$expiry, "%y"),
                toupper(x$right),
                x$strike,
                x$entry_price))
  } else {
    # Just month/day for near-term
    cat(sprintf("%s%s%s@%.2f", 
                format(x$expiry, "%b%d"), 
                toupper(x$right), 
                x$strike, 
                x$entry_price))
  }
  cat("\n")
  invisible(x)
}

#' Value of an option
#' 
#' Given an option object, a price of the underlying instrument, a time to maturity, and 
#' a value of the implied volatility, this method returns the corresponding value of the option.
#' 
#' @param object An object of class "option"
#' @param underlying Spot price of underlying
#' @param ttm Time to maturity in years
#' @param volatility Volatility (if NULL, uses stored iv)
#' @param r Risk‑free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @examples
#' # example code
#' may_put <- make_option(right = "p",
#'                        strike = 50,
#'                        expiry = as.Date("2026-05-15"),
#'                        style = "a",
#'                        commission = 1.00,
#'                        entry_price = 1.27,
#'                        date_of_entry = as.Date("2026-03-09)
#'                        underlying_at_entry = 51.7)
#' valuation_date <- as.Date("2026-04-15")
#' valuation_price <- 49.5
#' valuation_iv <- 0.35
#' valuation_ttm <- date_to_ttm (as.Date("2026-05-15"), valuation_date)
#' option.value(may_put,
#'              valuation_price,
#'              valuation_ttm,
#'              valuation_iv)
#' 
#' @return Numeric value
#' @export
option.value <- function(object,
                         underlying,
                         ttm,
                         volatility = NULL,
                         r = NULL,
                         q = 0,
                         ...) {
  UseMethod("option.value")
}

#' @export
option.value.option <- function(object,
                                underlying,
                                ttm,
                                volatility = NULL,
                                r = NULL,
                                q = 0,
                                ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  
  # If ttm is missing, calculate from object
  if (missing(ttm)) {
    ttm <- date_to_ttm(object$expiry)
  }
  
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$value
}

#' Delta of an option
#' 
#' Returns the option's delta (per-share sensitivity to $1 change in underlying).
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price
#' @param ttm Time to maturity in years (if missing, calculated)
#' @param volatility Volatility (if NULL, uses stored IV)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @return Numeric delta (per share)
#' 
#' @examples
#' opt <- make_option("c", strike=100, expiry="2026-12-31", 
#'                    entry_iv=0.2, underlying_at_entry=100)
#' option.delta(opt, underlying=105, ttm=0.5)
#' 
#' @export
option.delta <- function(object,
                         underlying,
                         ttm,
                         volatility = NULL,
                         r = NULL,
                         q = 0,
                         ...) {
  UseMethod("option.delta")
}

#' @export
option.delta.option <- function(object,
                                underlying,
                                ttm,
                                volatility = NULL,
                                r = NULL,
                                q = 0,
                                ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$delta
}

#' Gamma of an option
#' 
#' Returns the option's gamma (per-share sensitivity of delta to $1 change in underlying).
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price
#' @param ttm Time to maturity in years (if missing, calculated)
#' @param volatility Volatility (if NULL, uses stored IV)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @return Numeric gamma (per share)
#' 
#' @examples
#' opt <- make_option("c", strike=100, expiry="2026-12-31", 
#'                    entry_iv=0.2, underlying_at_entry=100)
#' option.gamma(opt, underlying=105, ttm=0.5)
#' 
#' @export
option.gamma <- function(object,
                         underlying,
                         ttm,
                         volatility = NULL,
                         r = NULL,
                         q = 0,
                         ...) {
  UseMethod("option.gamma")
}

#' @export
option.gamma.option <- function(object,
                                underlying,
                                ttm,
                                volatility = NULL,
                                r = NULL,
                                q = 0,
                                ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$gamma
}

#' Vega of an option
#' 
#' Returns the option's vega (per-share sensitivity to 1% change implied volatility).
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price
#' @param ttm Time to maturity in years (if missing, calculated)
#' @param volatility Volatility (if NULL, uses stored IV)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @return Numeric vega (per share)
#' 
#' @examples
#' opt <- make_option("c", strike=100, expiry="2026-12-31", 
#'                    entry_iv=0.2, underlying_at_entry=100)
#' option.vega(opt, underlying=105, ttm=0.5, volatility = 0.25)
#' 
#' @export
option.vega <- function(object,
                        underlying,
                        ttm,
                        volatility = NULL,
                        r = NULL,
                        q = 0,
                        ...) {
  UseMethod("option.vega")
}

#' @export
option.vega.option <- function(object,
                               underlying,
                               ttm,
                               volatility = NULL,
                               r = NULL,
                               q = 0,
                               ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$vega
}

#' Theta of an option
#' 
#' Returns the option's theta (per-share sensitivity to time decay, daily).
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price
#' @param ttm Time to maturity in years (if missing, calculated)
#' @param volatility Volatility (if NULL, uses stored IV)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @return Numeric theta (per share, daily)
#' 
#' @examples
#' opt <- make_option("c", strike=100, expiry="2026-12-31", 
#'                    entry_iv=0.2, underlying_at_entry=100)
#' option.theta(opt, underlying=105, ttm=0.5)
#' 
#' @export
option.theta <- function(object,
                         underlying,
                         ttm,
                         volatility = NULL,
                         r = NULL,
                         q = 0,
                         ...) {
  UseMethod("option.theta")
}

#' @export
option.theta.option <- function(object,
                                underlying,
                                ttm,
                                volatility = NULL,
                                r = NULL,
                                q = 0,
                                ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$theta
}

#' Rho of an option
#' 
#' Returns the option's rho (per-share sensitivity to 1% change of risk-free interest rate).
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price
#' @param ttm Time to maturity in years (if missing, calculated)
#' @param volatility Volatility (if NULL, uses stored IV)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' 
#' @return Numeric theta (per share, daily)
#' 
#' @examples
#' opt <- make_option("c", strike=100, expiry="2026-12-31", 
#'                    entry_iv=0.2, underlying_at_entry=100)
#' option.rho(opt, underlying=105, ttm=0.5, r = 0.06)
#' 
#' @export
option.rho <- function(object,
                       underlying,
                       ttm,
                       volatility = NULL,
                       r = NULL,
                       q = 0,
                       ...) {
  UseMethod("option.rho")
}

#' @export
option.rho.option <- function(object,
                              underlying,
                              ttm,
                              volatility = NULL,
                              r = NULL,
                              q = 0,
                              ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  res <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  res$rho
}

#' Theoretical price using stored volatility
#' 
#' Calculates the option's theoretical price using the stored implied volatility (IV).
#' 
#' @section Interpretation:
#' The meaning of this value depends on how the option was constructed:
#' \itemize{
#'   \item If only \code{entry_price} was provided (IV calculated): 
#'         Returns the model price consistent with the entry price (should match closely).
#'   \item If only \code{entry_iv} was provided (price calculated):
#'         Returns the price consistent with the given IV.
#'   \item If BOTH \code{entry_price} and \code{entry_iv} were provided:
#'         Returns the model price using the provided IV. This may differ from the 
#'         actual entry price, highlighting potential mispricing.
#' }
#' 
#' @return Numeric theoretical price (per share)
#' 
#' @examples
#' # On 09/03/2026 SPY closed at 678.27 and TWS is indicating an average IV of 20.3%
#' # The Jun18(2026) 275 call is selling for 31.00x31.46 (mid price = 31.23). Risk free
#' # rate is 4.5%, and SPY divident yield is 1.1%
#' set_global_r(0.045)
#' entry_date <- as.Date("2026-03-09")
#' june_call <- make_option(right = "c",
#'                          strike = 675,
#'                          expiry = as.Date("2026-06-18"),
#'                          style = "a",
#'                          entry_price = 31.23,
#'                          entry_iv = 0.203,
#'                          date_of_entry = entry_date,
#'                          underlying_at_entry = 678.27,
#'                          dividend_yield_at_entry = 0.011)
#' ttm <- date_to_ttm(as.Date("2026-06-18"), entry_date)
#' option.theor_price(june_call, 678.27, ttm)
#' @export
#' @export
option.theor_price <- function(object,
                               underlying,
                               ttm,
                               r = NULL,
                               q = 0,
                               ...) {
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  option.value(object, underlying, ttm, volatility = volatility, r = r, q = q, ...)
}

#' Intrinsic value
#' @export
option.intrinsic <- function(object, underlying) {
  if (object$right == "c") {
    max(underlying - object$strike, 0)
  } else {
    max(object$strike - underlying, 0)
  }
}

#' Extrinsic value = total value - intrinsic
#' @export
option.extrinsic <- function(object,
                             underlying,
                             ttm,
                             volatility = NULL,
                             r = NULL,
                             q = 0,
                             ...) {
  total <- option.value(object, underlying, ttm, volatility, r, q, ...)
  intrinsic <- option.intrinsic(object, underlying)
  total - intrinsic
}

#' Complete option analytics (value, greeks, and contract details)
#' 
#' Calculates all option metrics in a single call for efficiency.
#' Returns a data frame with one row containing:
#' - Contract details (right, strike, expiry, style, multiplier, commission)
#' - Market/entry information (entry_price, iv, underlying_at_entry, date_of_entry)
#' - Current valuation (value, delta, gamma, vega, theta, rho, dividendRho)
#' - Derived values (intrinsic, extrinsic, theor_price if applicable)
#' 
#' @param object An object of class "option"
#' @param underlying Current spot price (required)
#' @param ttm Time to maturity in years (if missing, calculated from object)
#' @param volatility Volatility (if NULL, uses stored iv)
#' @param r Risk‑free rate (if NULL, uses global)
#' @param q Dividend yield (default 0)
#' @param ... Additional arguments passed to price_option
#' @return A data frame with one row and multiple columns
#' @export
option.analytics <- function(object,
                             underlying,
                             ttm,
                             volatility = NULL,
                             r = NULL,
                             q = 0,
                             ...) {
  UseMethod("option.analytics")
}

#' @export
option.analytics.option <- function(object,
                                    underlying,
                                    ttm,
                                    volatility = NULL,
                                    r = NULL,
                                    q = 0,
                                    ...) {
  
  # --- Handle defaults ---
  if (is.null(volatility)) {
    volatility <- object$iv
  }
  
  if (missing(ttm)) {
    ttm <- date_to_ttm(object$expiry)
  }
  
  if (is.null(r)) {
    r <- get_global_r()
  }
  
  # --- Single valuation call (efficient!) ---
  valuation <- price_option(
    style = object$style,
    type = object$right,
    S = underlying,
    K = object$strike,
    r = r,
    q = q,
    T = ttm,
    iv = volatility,
    ...
  )
  
  # --- Derived values ---
  intrinsic <- if (object$right == "c") {
    max(underlying - object$strike, 0)
  } else {
    max(object$strike - underlying, 0)
  }
  extrinsic <- valuation$value - intrinsic
  
  # --- Theoretical price (if IV stored) ---
  theor_price <- if (!is.null(object$iv)) {
    option.value(object, underlying, ttm, volatility = object$iv, r = r, q = q, ...)
  } else {
    NA
  }
  
  # --- Build data frame ---
  data.frame(
    # Contract details
    right = object$right,
    strike = object$strike,
    expiry = as.character(object$expiry),  # Convert to string for clean printing
    style = object$style,
    multiplier = object$multiplier,
    commission = object$commission,
    
    # Entry information
    entry_price = ifelse(is.null(object$entry_price), NA, object$entry_price),
    iv_stored = object$iv,
    underlying_at_entry = ifelse(is.null(object$underlying_at_entry), NA, object$underlying_at_entry),
    date_of_entry = as.character(object$date_of_entry),
    dividend_yield_at_entry = object$dividend_yield_at_entry,
    
    # Current inputs
    current_underlying = underlying,
    current_ttm = ttm,
    current_vol = volatility,
    current_r = r,
    current_q = q,
    
    # Valuation results
    value = valuation$value,
    delta = valuation$delta,
    gamma = valuation$gamma,
    vega = valuation$vega,
    theta = valuation$theta,
    rho = valuation$rho,
    dividendRho = if (!is.null(valuation$dividendRho)) valuation$dividendRho else NA,
    
    # Derived metrics
    intrinsic = intrinsic,
    extrinsic = extrinsic,
    theor_price = theor_price,
    
    # Mispricing indicator (if both entry_price and iv_stored exist)
    mispricing = if (!is.null(object$entry_price) && !is.null(object$iv)) {
      theor_price - object$entry_price
    } else {
      NA
    },
    
    stringsAsFactors = FALSE
  )
}

# ---- Getters ----

#' @export
option.get_right <- function(object) object$right
#' @export
option.get_strike <- function(object) object$strike
#' @export
option.get_expiry <- function(object) object$expiry
#' @export
option.get_style <- function(object) object$style
#' @export
option.get_multiplier <- function(object) object$multiplier
#' @export
option.get_commission <- function(object) object$commission
#' @export
option.get_entry_price <- function(object) object$entry_price
#' @export
option.get_iv <- function(object) object$iv

# ============================================================================
# Underlying Class (stock, ETF, etc.)
# ============================================================================

#' Create an underlying instrument (stock, ETF, etc.)
#' 
#' @param symbol Character, identifier for the underlying (e.g., "AAPL", "SPY")
#' @param commission Numeric, per-share commission (default 0)
#' @param entry_price Optional numeric, price paid/received at trade inception
#' @param date_of_entry Date, trade inception date (default Sys.Date())
#' @return Object of class "underlying"
#' @export
make_underlying <- function(symbol,
                            commission = 0,
                            entry_price = NULL,
                            date_of_entry = Sys.Date()) {
  
  # Basic validation
  stopifnot(is.character(symbol), length(symbol) == 1)
  stopifnot(is.numeric(commission), length(commission) == 1, commission >= 0)
  
  if (!inherits(date_of_entry, "Date")) {
    stop("date_of_entry must be a Date object")
  }
  
  if (!is.null(entry_price)) {
    stopifnot(is.numeric(entry_price), length(entry_price) == 1)
  }
  
  structure(
    list(
      symbol = symbol,
      commission = commission,
      entry_price = entry_price,
      date_of_entry = date_of_entry
    ),
    class = "underlying"
  )
}

#' @export
print.underlying <- function(x, ...) {
  cat(sprintf("%s", x$symbol))
  if (!is.null(x$entry_price)) {
    cat(sprintf(" @ %.2f", x$entry_price))
  }
  cat(sprintf("\n  [comm: %.2f per share]", x$commission))
  invisible(x)
}

# ----------------------------------------------------------------------------
# Underlying Methods
# ----------------------------------------------------------------------------

#' Value of an underlying instrument (per share)
#' @param object An object of class "underlying"
#' @param price Current market price
#' @return Numeric value (per share)
#' @export
underlying.value <- function(object, price) {
  UseMethod("underlying.value")
}

#' @export
underlying.value.underlying <- function(object, price) {
  price
}

#' Delta of underlying (always 1 per share)
#' @export
underlying.delta <- function(object, ...) {
  UseMethod("underlying.delta")
}

#' @export
underlying.delta.underlying <- function(object, ...) {
  1
}

#' Gamma of underlying (always 0)
#' @export
underlying.gamma <- function(object, ...) {
  UseMethod("underlying.gamma")
}

#' @export
underlying.gamma.underlying <- function(object, ...) {
  0
}

#' Vega of underlying (always 0)
#' @export
underlying.vega <- function(object, ...) {
  UseMethod("underlying.vega")
}

#' @export
underlying.vega.underlying <- function(object, ...) {
  0
}

#' Theta of underlying (always 0 — no time decay)
#' @export
underlying.theta <- function(object, ...) {
  UseMethod("underlying.theta")
}

#' @export
underlying.theta.underlying <- function(object, ...) {
  0
}

#' Rho of underlying (always 0)
#' @export
underlying.rho <- function(object, ...) {
  UseMethod("underlying.rho")
}

#' @export
underlying.rho.underlying <- function(object, ...) {
  0
}

#' Entry cost for underlying position (per share)
#' @export
underlying.entry_cost <- function(object) {
  if (is.null(object$entry_price)) {
    return(0)
  }
  -object$entry_price  # Negative = money out
}

#' Complete analytics for underlying
#' @export
underlying.analytics <- function(object, price, ...) {
  UseMethod("underlying.analytics")
}

#' @export
underlying.analytics.underlying <- function(object, price, ...) {
  data.frame(
    symbol = object$symbol,
    commission = object$commission,
    entry_price = ifelse(is.null(object$entry_price), NA, object$entry_price),
    date_of_entry = as.character(object$date_of_entry),
    current_price = price,
    value = underlying.value(object, price),
    delta = 1,
    gamma = 0,
    vega = 0,
    theta = 0,
    rho = 0,
    stringsAsFactors = FALSE
  )
}