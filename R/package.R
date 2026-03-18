#' OptionSuite: Comprehensive Option Pricing and Analysis Suite
#'
#' @description
#' The OptionSuite package provides a complete toolkit for option pricing
#' and analysis, built with accuracy, flexibility, and real-world trading
#' applications in mind.
#'
#' @section Core Features:
#' * **Option Pricing**: European options via Black-Scholes (RQuantLib) and
#'   American options via Bjerksund-Stensland 2002 closed-form approximation
#'   (from the fOptions package with modifications)
#' * **Full Greeks**: Delta, gamma, vega, theta, rho for all option types
#' * **Unified Instrument API**: Common interface for options and underlyings
#'   through the `instrument` superclass
#' * **Flexible Input**: Three construction modes (price only, IV only, or both)
#'   enabling mispricing analysis
#' * **Intelligent Defaults**: Methods automatically use stored values when
#'   parameters are omitted
#' * **Expiry Handling**: Accepts Date objects, character strings, or numeric
#'   year fractions
#' * **Global Settings**: Centralized control of risk-free rate, day-count
#'   convention (365 days or 252 sessions), and commission handling
#'
#' @section Main Functions:
#' * **Option creation**: `make_option()`
#' * **Underlying creation**: `make_underlying()`
#' * **Pricing engine**: `price_option()`
#' * **Unified instrument methods**: `instrument.value()`, `instrument.delta()`,
#'   `instrument.gamma()`, `instrument.vega()`, `instrument.theta()`,
#'   `instrument.rho()`, `instrument.theor_price()`, `instrument.intrinsic()`,
#'   `instrument.extrinsic()`, `instrument.analytics()`
#' * **Global settings**: `set_global_r()`, `set_day_count_convention()`,
#'   `set_min_reliable_ttm()`, `set_use_commission()`
#'
#' @section Package Philosophy:
#' The package is designed with a clean separation between:
#' * **Instruments** (options, underlyings) — what they are, with a unified
#'   API through the `instrument` superclass
#' * **Positions** (legs) — what you hold (to be added in future versions)
#' * **Strategies** (multi-leg) — what you trade (to be added)
#'
#' This foundation ensures that all calculations are consistent, testable,
#' and suitable for both academic and professional use. All methods support
#' intelligent defaults: if you created an instrument with all entry data,
#' you can simply call `instrument.value(my_opt)` without any parameters.
#'
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

## usethis namespace: start
#' @import RQuantLib
#' @import fOptions
#' @importFrom stats pnorm
## usethis namespace: end
NULL