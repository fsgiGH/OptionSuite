# legs.R
# Leg class for OptionSuite v0.2.0
# A leg combines an instrument with quantity and direction

#' Create a leg (position) from an instrument
#' 
#' A leg represents a trading position in an instrument, combining the instrument
#' itself with the quantity traded and the direction (buy or sell). The leg
#' inherits all properties of the underlying instrument (multiplier, commission,
#' etc.) and adds position-specific information.
#' 
#' @param instrument An object of class "instrument" (option or underlying)
#' @param quantity Numeric, number of contracts/shares (must be positive)
#' @param direction Character, either "buy" (long) or "sell" (short)
#' 
#' @details
#' The leg stores the instrument and position details. Key properties:
#' * For options: quantity refers to number of contracts (each contract = multiplier shares)
#' * For underlyings: quantity refers to number of shares
#' * Commission from the instrument is applied per transaction, not per unit
#' * Cash flow is calculated as: -quantity * instrument$entry_price * multiplier for buys,
#'   and +quantity * instrument$entry_price * multiplier for sells
#' 
#' @return An object of class "leg" containing the instrument and position data
#' 
#' @examples
#' # Create an option first
#' call_opt <- make_option(
#'   right = "c",
#'   strike = 100,
#'   expiry = as.Date("2026-12-31"),
#'   style = "e",
#'   entry_price = 10,
#'   entry_iv = 0.2,
#'   underlying_at_entry = 100
#' )
#' 
#' # Create a leg (buy 10 call options)
#' leg1 <- make_leg(call_opt, quantity = 10, direction = "buy")
#' print(leg1)
#' 
#' # Create a leg (sell 5 call options)
#' leg2 <- make_leg(call_opt, quantity = 5, direction = "sell")
#' 
#' # Create an underlying and leg
#' spy <- make_underlying(symbol = "SPY", entry_price = 450)
#' leg3 <- make_leg(spy, quantity = 100, direction = "buy")
#' 
#' @export
make_leg <- function(instrument, quantity, direction = c("buy", "sell")) {
  # Validate inputs
  stopifnot(inherits(instrument, "instrument"))
  stopifnot(is.numeric(quantity), length(quantity) == 1, quantity > 0)
  direction <- match.arg(direction)
  
  # Create leg object
  structure(
    list(
      instrument = instrument,
      quantity = quantity,
      direction = direction,
      # Cache frequently used instrument properties for performance
      multiplier = if (inherits(instrument, "option")) instrument$multiplier else 1,
      commission = instrument$commission
    ),
    class = "leg"
  )
}

#' @export
print.leg <- function(x, ...) {
  # Format direction symbol
  dir_symbol <- if (x$direction == "buy") "+" else "-"
  
  # Format instrument description (reuse its print method)
  inst_desc <- capture.output(print(x$instrument))
  
  cat(sprintf("%s%s x %d %s\n", 
              dir_symbol,
              format(x$quantity, big.mark = ","),
              x$multiplier,
              if (inherits(x$instrument, "option")) "contracts" else "shares"))
  cat("  Instrument: ", inst_desc[1], "\n", sep = "")
  if (length(inst_desc) > 1) {
    cat("  ", inst_desc[-1], "\n", sep = "")
  }
  invisible(x)
}

#' Calculate cash flow for a leg at entry
#' 
#' The cash flow represents the net amount of money exchanged when opening
#' the position. For buys (long), cash flow is negative (money paid out).
#' For sells (short), cash flow is positive (money received).
#' 
#' @param leg An object of class "leg"
#' @return Numeric, total cash flow for the position (including multiplier)
#' 
#' @examples
#' call_opt <- make_option(
#'   right = "c",
#'   strike = 100,
#'   expiry = as.Date("2026-12-31"),
#'   entry_price = 10,
#'   underlying_at_entry = 100
#' )
#' 
#' # Buy 10 calls: cash flow = -10 * 10 * 100 = -10,000
#' leg_buy <- make_leg(call_opt, quantity = 10, direction = "buy")
#' leg.cash_flow(leg_buy)
#' 
#' # Sell 5 calls: cash flow = +5 * 10 * 100 = +5,000  
#' leg_sell <- make_leg(call_opt, quantity = 5, direction = "sell")
#' leg.cash_flow(leg_sell)
#' 
#' @export
leg.cash_flow <- function(leg) {
  UseMethod("leg.cash_flow")
}

#' @export
leg.cash_flow.leg <- function(leg) {
  # Get entry price (NULL if not available)
  entry_price <- if (inherits(leg$instrument, "option")) {
    leg$instrument$entry_price
  } else {
    leg$instrument$entry_price
  }
  
  # If no entry price, cash flow is 0
  if (is.null(entry_price)) {
    warning("No entry price available for instrument, cash flow = 0")
    return(0)
  }
  
  # Calculate raw cash flow (without commission)
  sign <- ifelse(leg$direction == "buy", -1, 1)
  raw_cf <- sign * leg$quantity * entry_price * leg$multiplier
  
  # Add commission if enabled
  if (get_use_commission()) {
    # Commission always reduces cash flow
    raw_cf <- raw_cf - leg$commission
  }
  
  return(raw_cf)
}

#' Get instrument from a leg
#' 
#' @param leg An object of class "leg"
#' @return The underlying instrument object
#' 
#' @examples
#' opt <- make_option(right = "c", strike = 100, expiry = "2026-12-31",
#'                   entry_price = 10, underlying_at_entry = 100)
#' leg <- make_leg(opt, quantity = 10, direction = "buy")
#' leg.get_instrument(leg)
#' 
#' @export
leg.get_instrument <- function(leg) {
  leg$instrument
}

#' Get quantity from a leg
#' 
#' @param leg An object of class "leg"
#' @return Numeric quantity (always positive)
#' 
#' @examples
#' opt <- make_option(right = "c", strike = 100, expiry = "2026-12-31",
#'                   entry_price = 10, underlying_at_entry = 100)
#' leg <- make_leg(opt, quantity = 10, direction = "buy")
#' leg.get_quantity(leg)
#' 
#' @export
leg.get_quantity <- function(leg) {
  leg$quantity
}

#' Get direction from a leg
#' 
#' @param leg An object of class "leg"
#' @return Character "buy" or "sell"
#' 
#' @examples
#' opt <- make_option(right = "c", strike = 100, expiry = "2026-12-31",
#'                   entry_price = 10, underlying_at_entry = 100)
#' leg <- make_leg(opt, quantity = 10, direction = "buy")
#' leg.get_direction(leg)
#' 
#' @export
leg.get_direction <- function(leg) {
  leg$direction
}

#' Get effective multiplier for a leg
#' 
#' The effective multiplier is the instrument's multiplier (100 for options, 1 for underlyings)
#' 
#' @param leg An object of class "leg"
#' @return Numeric multiplier
#' 
#' @examples
#' opt <- make_option(right = "c", strike = 100, expiry = "2026-12-31",
#'                   multiplier = 100, entry_price = 10, underlying_at_entry = 100)
#' leg <- make_leg(opt, quantity = 10, direction = "buy")
#' leg.get_multiplier(leg)  # Returns 100
#' 
#' @export
leg.get_multiplier <- function(leg) {
  leg$multiplier
}