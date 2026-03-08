---
title: "DECISIONS"
output: html_document
---

# OptionSuite Design Decisions Record

This document tracks important design decisions made during development to ensure consistency and provide context for future maintainers.

---

## Greeks Display Convention

**Date:** 2026-03-07

**Context:** 
When building synthetic positions, we need to align with real-world broker displays (like TWS and ToS). In TWS:
- An ATM call option shows delta ≈ 0.52 (contract-level)
- Adding 100 shares increases total delta to 1.52 (0.52 + 1.00)
- Adding 1 share increases total delta to 0.53 (0.52 + 0.01)

**Decision:**
- **Option Greeks** are stored and calculated at **per-share** level internally (e.g., ATM call delta ≈ 0.0052)
- **Underlying Greeks** are stored at **per-share** level (delta = 1.0)
- **Position-level display** (to be implemented in future leg/strategy classes) will:
  - Show options at **contract-level** (per-share × 100)
  - Show underlyings at **share-level** (per-share × 1)
  - Sum positions appropriately for total portfolio view

**Rationale:**
- **Mathematical consistency**: All instruments priced per-share internally
- **User familiarity**: Matches broker displays (TWS, ToS) that traders are accustomed to
- **Clean separation**: Calculation (per-share) vs. presentation (contract/share level)
- **Flexibility**: Easy to scale for different multipliers or position sizes

**Example:**
```r
# Internal calculation
option.delta(opt)           # 0.0052 (per share)
underlying.delta(stock)      # 1.0 (per share)

# Position display (future)
position.delta(opt_leg)      # Shows 0.52 (contract-level)
position.delta(stk_leg)      # Shows 1.0 (share-level)
position.delta(total)        # Shows 1.52 (correct sum)

**Implementation Status:** To be implemented in future leg/strategy classes (planned for Version 0.2.0).

---

## Day-Count Convention

**Date:** 2026-03-07

**Context:** 
Different platforms use different conventions for time-to-maturity calculations:
- ToS uses 252 trading days
- TWS uses 365 calendar days
This affects implied volatility values even for the same option.

**Decision:**
- `date_to_ttm()` always returns **calendar years** (days/365)
- Day-count convention affects **volatility scaling** in pricing functions:
  - If `"trading/252"`: Input volatility is scaled by √(365/252) before pricing
  - If `"calendar/365"`: Input volatility used as-is
- Global setting `set_day_count_convention()` controls this behavior

**Rationale:**
- Keeps TTM calculation simple and consistent
- Isolates convention differences to volatility scaling only
- User inputs IV exactly as reported by their broker

**Example:**
```r
set_day_count_convention("trading/252")
price_option(..., sigma = 0.30)  # 30% IV from ToS automatically scaled
**Implementation Status:** Implemented in `valuation.R` (`price_option` dispatcher).

---

## Minimum Reliable TTM Threshold

**Date:** 2026-03-06

**Context:** 
Empirical testing showed RQuantLib returns zeros for all outputs when `ttm < 0.0014` years (≈0.5 trading days).

**Decision:**
- For `ttm ≤ MIN_RELIABLE_TTM` (default 0.0014):
  - **Value** = exact payoff (intrinsic value)
  - **Greeks** = calculated at `ttm = MIN_RELIABLE_TTM` for numerical stability
- Global setting `set_min_reliable_ttm()` allows adjustment

**Rationale:**
- Avoids numerical instability in RQuantLib
- Provides sensible Greeks near expiration
- Documented, predictable behavior

**Implementation Status:** Implemented in `valuation.R` (`price_european`, `price_american`).

---

## Option Construction Modes

**Date:** 2026-03-06

**Context:** 
Users may have different input data available (market price, implied volatility, or both).

**Decision:**
Three construction modes in `make_option()`:
1. **Price only** (`entry_price` provided, `entry_iv = NULL`): 
   - IV calculated via RQuantLib implied volatility functions
2. **IV only** (`entry_iv` provided, `entry_price = NULL`):
   - Theoretical price calculated via pricing model
3. **Both provided**:
   - Both values stored as-is
   - Enables mispricing analysis via `option.theor_price()` vs. `option.market_price()`

**Rationale:**
- Maximum flexibility for different use cases
- Mispricing detection as a built-in feature
- Clear, documented behavior

**Implementation Status:** Fully implemented in `instruments.R`.

---

## Future Directions

### Version 0.2.0 (Planned)
- Leg class for positions (quantity + instrument)
- Strategy class for multi-leg positions
- PnL calculations with commissions
- Position-level Greeks with proper display scaling

### Version 0.3.0 (Planned)
- Greeks curves across spot prices
- Probability of profit calculations
- Convexity metrics (TCI/CCI)

### Version 0.4.0 (Planned)
- Visualization functions
- Interactive charts
- Report generation

---

*This file will be updated as new design decisions are made.*