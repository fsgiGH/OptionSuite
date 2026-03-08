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
#' * **Full Greeks**: Delta, gamma, vega, theta, rho for all option types
#' * **Flexible Input**: Three construction modes (price only, IV only, or both)
#'   enabling mispricing analysis
#' * **Expiry Handling**: Accepts Date objects, character strings, or numeric
#'   year fractions
#' * **Global Settings**: Centralized control of risk-free rate and day-count
#'   convention (365 days or 252 sessions)
#'
#' @section Main Functions:
#' * **Option creation**: `make_option()`
#' * **Underlying creation**: `make_underlying()`
#' * **Pricing engine**: `price_option()`, `price_european()`, `price_american()`
#' * **Option methods**: `option.value()`, `option.delta()`, `option.gamma()`,
#'   `option.vega()`, `option.theta()`, `option.rho()`, `option.theor_price()`,
#'   `option.analytics()`
#' * **Underlying methods**: `underlying.value()`, `underlying.delta()`, etc.
#' * **Global settings**: `set_global_r()`, `set_day_count_convention()`,
#'   `set_min_reliable_ttm()`
#'
#' @section Package Philosophy:
#' The package is designed with a clean separation between:
#' * **Instruments** (options, underlyings) — what they are
#' * **Positions** (legs) — what you hold (to be added in future versions)
#' * **Strategies** (multi-leg) — what you trade (to be added)
#'
#' This foundation ensures that all calculations are consistent, testable,
#' and suitable for both academic and professional use.
#'
#' @docType package
#' @name OptionSuite-package
#' @aliases OptionSuite
#' @useDynLib OptionSuite, .registration = TRUE
#' @import RQuantLib
#' @importFrom stats pnorm
NULL