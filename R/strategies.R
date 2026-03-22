# strategies.R
# Strategy class for OptionSuite v0.2.0
# A strategy combines multiple legs into a single trading position

#' Create a strategy from a list of legs
#' 
#' A strategy represents a multi-leg trading position (e.g., covered call, straddle,
#' iron condor). It is simply a collection of legs that are traded together.
#' The strategy inherits no additional properties but provides methods for
#' aggregated calculations across all legs.
#' 
#' @param legs A list of objects of class "leg"
#' @param name Optional character, a descriptive name for the strategy
#' 
#' @return An object of class "strategy" containing the legs
#' 
#' @examples
#' # Create a covered call strategy (long stock + short call)
#' stock <- make_underlying(symbol = "SPY", entry_price = 450)
#' call <- make_option(
#'   right = "c",
#'   strike = 460,
#'   expiry = as.Date("2026-12-31"),
#'   style = "a",
#'   entry_price = 5,
#'   underlying_at_entry = 450
#' )
#' 
#' leg1 <- make_leg(stock, quantity = 100, direction = "buy")
#' leg2 <- make_leg(call, quantity = 1, direction = "sell")
#' 
#' covered_call <- make_strategy(
#'   legs = list(leg1, leg2),
#'   name = "SPY Covered Call"
#' )
#' print(covered_call)
#' 
#' @export
make_strategy <- function(legs, name = NULL) {
  # Validate inputs
  stopifnot(is.list(legs))
  if (length(legs) == 0) {
    stop("Strategy must contain at least one leg")
  }
  for (i in seq_along(legs)) {
    if (!inherits(legs[[i]], "leg")) {
      stop(sprintf("Element %d is not a leg object", i))
    }
  }
  
  # Create strategy object
  structure(
    list(
      legs = legs,
      name = name %||% "Strategy",
      date_created = Sys.Date(),
      n_legs = length(legs)
    ),
    class = "strategy"
  )
}

#' @export
print.strategy <- function(x, ...) {
  cat(sprintf("%s\n", x$name))
  cat(sprintf("  Created: %s\n", format(x$date_created)))
  cat(sprintf("  Legs: %d\n", x$n_legs))
  cat("\n")
  
  for (i in seq_along(x$legs)) {
    cat(sprintf("Leg %d: ", i))
    print(x$legs[[i]])
  }
  invisible(x)
}

#' Calculate total cash flow for a strategy at entry
#' 
#' The strategy cash flow is simply the sum of cash flows from all legs.
#' This represents the net amount paid (negative) or received (positive)
#' when opening the entire strategy.
#' 
#' @param strategy An object of class "strategy"
#' @return Numeric, total cash flow for the strategy
#' 
#' @examples
#' # Covered call example
#' stock <- make_underlying(symbol = "SPY", entry_price = 450)
#' call <- make_option(
#'   right = "c", strike = 460, expiry = "2026-12-31",
#'   entry_price = 5, underlying_at_entry = 450
#' )
#' 
#' leg1 <- make_leg(stock, quantity = 100, direction = "buy")
#' leg2 <- make_leg(call, quantity = 1, direction = "sell")
#' 
#' covered_call <- make_strategy(list(leg1, leg2), "Covered Call")
#' 
#' # Cash flow: -45,000 (buy stock) + 500 (sell call) = -44,500
#' strategy.cash_flow(covered_call)
#' 
#' @export
strategy.cash_flow <- function(strategy) {
  UseMethod("strategy.cash_flow")
}

#' @export
strategy.cash_flow.strategy <- function(strategy) {
  sum(sapply(strategy$legs, leg.cash_flow))
}

#' Calculate value of a strategy at a given valuation date
#' 
#' The strategy value is the sum of leg values, respecting direction
#' (buy = +value, sell = -value). Each leg calculates its own time-to-maturity
#' based on its instrument's expiry and the provided valuation date.
#' 
#' @param strategy An object of class "strategy"
#' @param underlying Current spot price (required)
#' @param date Valuation date (Date object or character YYYY-MM-DD)
#' @param iv Implied volatility (if NULL, uses stored IVs)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (if NULL, uses stored values)
#' 
#' @return Numeric, total strategy value
#' 
#' @examples
#' # Create a calendar spread (short near-term, long far-term)
#' near_call <- make_option(
#'   right = "c", strike = 100,
#'   expiry = as.Date("2026-06-30"),
#'   entry_price = 5, underlying_at_entry = 100
#' )
#' far_call <- make_option(
#'   right = "c", strike = 100,
#'   expiry = as.Date("2026-12-31"),
#'   entry_price = 10, underlying_at_entry = 100
#' )
#' 
#' leg1 <- make_leg(near_call, quantity = 1, direction = "sell")
#' leg2 <- make_leg(far_call, quantity = 1, direction = "buy")
#' 
#' calendar <- make_strategy(list(leg1, leg2), "Call Calendar Spread")
#' 
#' # Value at mid-point date
#' strategy.value(calendar, underlying = 105, date = "2026-09-30")
#' 
#' @export
strategy.value <- function(strategy, underlying, date, iv = NULL, r = NULL, q = NULL) {
  UseMethod("strategy.value")
}

#' @export
strategy.value.strategy <- function(strategy, underlying, date, iv = NULL, r = NULL, q = NULL) {
  # Convert date if character
  if (is.character(date)) {
    date <- as.Date(date)
  }
  stopifnot(inherits(date, "Date"))
  
  total <- 0
  for (leg in strategy$legs) {
    # For options: calculate TTM based on expiry
    # For underlyings: TTM is not applicable (use 0 or ignore)
    if (inherits(leg$instrument, "option")) {
      ttm <- date_to_ttm(leg$instrument$expiry, date)
    } else {
      # Underlying has no expiry - TTM is 0 (no time decay)
      ttm <- 0
    }
    
    leg_value <- instrument.value(
      leg$instrument,
      underlying = underlying,
      ttm = ttm,
      iv = iv,
      r = r,
      q = q
    )
    
    # Apply direction and quantity
    sign <- ifelse(leg$direction == "buy", 1, -1)
    total <- total + sign * leg$quantity * leg$multiplier * leg_value
  }
  return(total)
}

#' Calculate payoff of a strategy at a given valuation date
#' 
#' The strategy payoff is the strategy value PLUS the total cash flow
#' from entry. This represents the profit/loss of the position.
#' 
#' @param strategy An object of class "strategy"
#' @param underlying Current spot price (required)
#' @param date Valuation date (Date object or character YYYY-MM-DD)
#' @param iv Implied volatility (if NULL, uses stored IVs)
#' @param r Risk-free rate (if NULL, uses global)
#' @param q Dividend yield (if NULL, uses stored values)
#' 
#' @return Numeric, total strategy payoff (P&L)
#' 
#' @examples
#' # Calendar spread P&L at expiration of near-term option
#' near_call <- make_option(
#'   right = "c", strike = 100,
#'   expiry = as.Date("2026-06-30"),
#'   entry_price = 5, underlying_at_entry = 100
#' )
#' far_call <- make_option(
#'   right = "c", strike = 100,
#'   expiry = as.Date("2026-12-31"),
#'   entry_price = 10, underlying_at_entry = 100
#' )
#' 
#' leg1 <- make_leg(near_call, quantity = 1, direction = "sell")
#' leg2 <- make_leg(far_call, quantity = 1, direction = "buy")
#' 
#' calendar <- make_strategy(list(leg1, leg2), "Call Calendar Spread")
#' 
#' # P&L at near-term expiration
#' strategy.payoff(calendar, underlying = 105, date = "2026-06-30")
#' 
#' @export
strategy.payoff <- function(strategy, underlying, date, iv = NULL, r = NULL, q = NULL) {
  UseMethod("strategy.payoff")
}

#' @export
strategy.payoff.strategy <- function(strategy, underlying, date, iv = NULL, r = NULL, q = NULL) {
  value <- strategy.value(strategy, underlying, date, iv, r, q)
  cash_flow <- strategy.cash_flow(strategy)
  return(value + cash_flow)
}

#' Get legs from a strategy
#' 
#' @param strategy An object of class "strategy"
#' @return List of leg objects
#' 
#' @examples
#' covered_call <- make_strategy(list(leg1, leg2))
#' legs <- strategy.get_legs(covered_call)
#' 
#' @export
strategy.get_legs <- function(strategy) {
  strategy$legs
}

#' Get number of legs in a strategy
#' 
#' @param strategy An object of class "strategy"
#' @return Integer number of legs
#' 
#' @examples
#' covered_call <- make_strategy(list(leg1, leg2))
#' strategy.n_legs(covered_call)  # Returns 2
#' 
#' @export
strategy.n_legs <- function(strategy) {
  strategy$n_legs
}

#' Get strategy name
#' 
#' @param strategy An object of class "strategy"
#' @return Character strategy name
#' 
#' @examples
#' covered_call <- make_strategy(list(leg1, leg2), "Covered Call")
#' strategy.get_name(covered_call)
#' 
#' @export
strategy.get_name <- function(strategy) {
  strategy$name
}

#' Set strategy name
#' 
#' @param strategy An object of class "strategy"
#' @param name New name for the strategy
#' @return Modified strategy object (invisibly)
#' 
#' @examples
#' covered_call <- make_strategy(list(leg1, leg2))
#' strategy.set_name(covered_call, "My Covered Call")
#' 
#' @export
strategy.set_name <- function(strategy, name) {
  strategy$name <- name
  invisible(strategy)
}