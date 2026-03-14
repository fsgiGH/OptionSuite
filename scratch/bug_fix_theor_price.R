# On 09/03/2026 SPY closed at 678.27 and TWS is indicating an average IV of 20.3%
#' # The Jun18(2026) 275 call is selling for 31.00x31.46 (mid price = 31.23). Risk free
#' # rate is 4.5%, and SPY divident yield is 1.1%
library(devtools)
devtools::load_all()
set_global_r(0.045)
entry_date <- as.Date("2026-03-09")
june_call <- make_option(right = "c",
                        strike = 675,
                        expiry = as.Date("2026-06-18"),
                        style = "a",
                        entry_price = 31.23,
                        entry_iv = 0.203,
                        date_of_entry = entry_date,
                        underlying_at_entry = 678.27,
                        dividend_yield_at_entry = 0.011)
ttm <- date_to_ttm(as.Date("2026-06-18"), entry_date)
option.value(june_call, 678.27, ttm, 0.203)
option.delta(june_call, 678.27,ttm, 0.203)
option.theor_price(june_call, 678.27, ttm)
