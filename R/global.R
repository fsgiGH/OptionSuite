# global.R
# Global settings for the OptionSuite package

.optionsuite <- new.env(parent = emptyenv())

# Default values
.optionsuite$r <- 0.05                       # risk‑free rate (5%)
.optionsuite$day_count <- "calendar/365"     # "calendar/365" or "trading/252"
.optionsuite$min_reliable_ttm <- 0.0014      # from empirical testing
.optionsuite$use_commission <- FALSE

#' Set whether to include commission in calculations. At startup the Suite ignores
#' commissions so a call to this function is needed to change the default behavior
#' of the suite.
#' @param use Logical, TRUE to include commissions (DEFAULT), FALSE to ignore.
#' @examples
#' set_use_commission(TRUE)
#' 
#' @export
set_use_commission <- function(use = TRUE) {
  stopifnot(is.logical(use), length(use) == 1)
  .optionsuite_comission <- use
  invisible()
}

#' Get current commission setting
#' @return Logical, TRUE if commissions are enabled
#' @examples
#' get_use_commission()
#' @export
get_use_commission <- function() {
  .optionsuite$use_commission
}

#' 
#' Set global risk‑free rate
#' 
#' @param rate Numeric, annualised rate (e.g., 0.05 for 5%)
#' 
#' @examples
#' get_global_r()
#' set_global_r(0.044)
#' get_global_r()
#' 
#' @seealso \code{\link{get_global_r}} to see the currently used risk-free rate.
#' 
#' @export
set_global_r <- function(rate) {
  stopifnot(is.numeric(rate), length(rate) == 1, rate >= -1)
  .optionsuite$r <- rate
  invisible()
}

#' Get global risk‑free rate
#' 
#' @return Numeric, the current global risk-free rate (e.g. 0.05 for 5%)
#' 
#' @examples
#' # Get current global rate
#' get_global_r()
#' 
#' # Set a new rate and verify
#' old_rate <- get_global_r()
#' set_global_r(0.03)
#' get_global_r()
#' set_global_r(old_rate) # restore
#' 
#' @seealso \code{\link{set_global_r}} to set risk-free rate.
#' 
#' @export
get_global_r <- function() {
  .optionsuite$r
}

#' Set day‑count convention for volatility annualization.
#' 
#' Different brokers use different conventions. For instance Think Or Swim (Schwab) apparently uses the 252 days convention
#' (trading sessions in a year, on average) while IBKR (TWS) apparently uses 365 days (approximate days in one year) for
#' implied volatility and option pricing calculations. OptionSuite allows the user to adopt one or the other scheme, interchangeably.
#' 
#' @param convention Character, either "calendar/365" or "trading/252"
#' 
#' @examples
#' set_day_count_convention("calendar/365")
#' get_day_count_convention()
#' set_day_count_convention("trading/252")
#' get_day_count_convention()
#' 
#' @seealso \code{\link{get_day_count_convention}} to see how this parameter is currently set.
#' 
#' @export
set_day_count_convention <- function(convention = c("calendar/365", "trading/252")) {
  convention <- match.arg(convention)
  .optionsuite$day_count <- convention
  invisible()
}

#' Get current day‑count convention for volatility annualization.
#' 
#' Different brokers use different conventions. For instance Think Or Swim (Schwab) apparently uses the 252 days convention
#' (trading sessions in a year, on average) while IBKR (TWS) apparently uses 365 days (approximate days in one year) for
#' implied volatility and option pricing calculations. OptionSuite allows the user to adopt one or the other scheme, interchangeably.
#' 
#' @return Character either "calendar/365" or "trading/252"
#' 
#' @examples
#' set_day_count_convention("calendar/365")
#' get_day_count_convention()
#' set_day_count_convention("trading/252")
#' get_day_count_convention()
#' 
#' @seealso \code{\link{set_day_count_convention}} to change this parameter setting.
#' @export
get_day_count_convention <- function() {
  .optionsuite$day_count
}

#' Set minimum reliable TTM for numerical pricing engines
#' 
#' OptionSuite relies, for several option valuation calculations, on publicly available libraries like QuantLib. During development, empirically,
#' it has been discovered that these algorithms do not provide valid results for time to maturity values below a certain threshold value (around)
#' 0.0014. It is then advisable to globally set the minimal time to maturity level used by OptionsSuite calculation to this small value not to
#' run into the risk of obtaining unreliable values. But the user is free to bypass this constraint, under her own responsibility.
#' 
#' @param ttm Numeric, positive the choosen minimal time to maturity value use for option value calculations
#' 
#' @examples
#' old_min_ttm <- get_min_reliable_ttm
#' set_min_reliable_ttm(0.025)
#' get_min_reliable_ttm()
#' set_min_reliable_ttm(old_min_ttm) # restore
#' 
#' @seealso \code{\link{get_min_reliable_ttm}} to see how this value is currently set.
#' 
#' @export
set_min_reliable_ttm <- function(ttm) {
  stopifnot(is.numeric(ttm), length(ttm) == 1, ttm > 0)
  .optionsuite$min_reliable_ttm <- ttm
  invisible()
}

#' Get minimum reliable TTM
#' 
#' OptionSuite relies, for several option valuation calculations, on publicly available libraries like QuantLib. During development, empirically,
#' it has been discovered that these algorithms do not provide valid results for time to maturity values below a certain threshold value (around)
#' 0.0014. It is then advisable to globally set the minimal time to maturity level used by OptionsSuite calculation to this small value not to
#' run into the risk of obtaining unreliable values. But the user is free to bypass this constraint, under her own responsibility.
#' 
#' This function shows the minimal time to maturity currently in use.
#' 
#' @examples
#' old_min_ttm <- get_min_reliable_ttm
#' set_min_reliable_ttm(0.025)
#' get_min_reliable_ttm()
#' set_min_reliable_ttm(old_min_ttm) # restore
#' 
#' @seealso \code{\link{set_min_reliable_ttm}} to modify this value.
#' 
#' @return Numeric The minimum time to maturity value currently in use
#' @export
get_min_reliable_ttm <- function() {
  .optionsuite$min_reliable_ttm
}

#' Convert dates to time‑to‑maturity (always calendar years)
#' 
#' Given an expiry, passed as a numeric (fraction of a year of 365 days or as a Date object), and an optional valuation date as a
#' Date object (by default the current system date will be used) the function returns the time to maturity of the option as a fraction of
#' the calendar year (365 days)
#' 
#' @param expiry Date, character (YYYY-MM-DD) or numeric (if numeric, returned unchanged)
#' @param valuation_date Date, character (YYYY-MM-DD) or missing (uses Sys.Date)
#' 
#' @return Time in years (calendar)
#' 
#' @examples
#' date_to_ttm(0.5)
#' #option expiry
#' option_maturity <- as.Date("2026-04-17")
#' #valuation day
#' today <- "2026-03-08"
#' date_to_ttm(option_maturity, today)
#' 
#' @export
date_to_ttm <- function(expiry, valuation_date = Sys.Date()) {
  # Handle character expiry
  if (is.character(expiry)) {
    expiry <- as.Date(expiry)
  }
  
  # Handle character valuation_date
  if (is.character(valuation_date)) {
    valuation_date <- as.Date(valuation_date)
  }
  
  if (is.numeric(expiry)) {
    return(expiry)
  }
  if (!inherits(expiry, "Date")) {
    stop("expiry must be Date, character (YYYY-MM-DD), or numeric")
  }
  if (!inherits(valuation_date, "Date")) {
    stop("valuation_date must be Date or character (YYYY-MM-DD)")
  }
  
  days <- as.numeric(difftime(expiry, valuation_date, units = "days"))
  return(days / 365)
}