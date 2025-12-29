# =============================================================================
# SCENARIOS: mortgage, rent, cash
# =============================================================================

# -----------------------------------------------------------------------------
# Mortgage scenario
# -----------------------------------------------------------------------------
run_mortgage <- function(sim, paths, shocks, p, dp_pct = NULL) {

  years <- p$mortgage_years
  dp <- ifelse(is.null(dp_pct), p$down_payment_pct, dp_pct)

  wealth <- liquid <- house <- debt <- numeric(years + 1)

  # --- Initial conditions
  down_payment <- dp * p$house_price
  loan <- p$house_price - down_payment
  notary <- p$house_price * p$notary_fees_pct

  liquid[1] <- p$initial_capital - down_payment - notary
  house[1] <- p$house_price
  debt[1] <- loan
  wealth[1] <- liquid[1] + house[1] - debt[1]

  # --- Mortgage payment
  r_m <- p$mortgage_rate / 12
  n_pay <- years * 12
  monthly <- loan * (r_m * (1 + r_m)^n_pay) / ((1 + r_m)^n_pay - 1)
  annual_p <- monthly * 12

  salary <- p$annual_salary

  # --- Time loop
  for (t in 2:(years + 1)) {
    y <- t - 1

    # Salary evolution
    salary <- salary * (1 + paths$salary[y, sim])
    savings <- salary * p$savings_rate

    # Owner costs
    owner_cost <- p$owner_yearly_charge *
      prod(1 + paths$infl[1:y, sim])

    # Mortgage interest and tax benefit
    interest <- debt[t - 1] * p$mortgage_rate
    deductible <- min(interest, p$max_deductible_interest)
    tax_benefit <- deductible * p$mortgage_interest_deduction_rate

    # Liquid capital
    liquid[t] <- liquid[t - 1] * (1 + paths$inv[y, sim]) +
      savings -
      annual_p -
      owner_cost +
      tax_benefit -
      shocks[t, sim]

    # Debt evolution
    debt[t] <- max(
      0,
      debt[t - 1] * (1 + r_m)^12 -
        monthly * ((1 + r_m)^12 - 1) / r_m
    )

    # House value
    house[t] <- house[t - 1] * (1 + paths$house[y, sim])

    # Total wealth
    wealth[t] <- liquid[t] + house[t] - debt[t]
  }

  list(
    scenario = "Mortgage",
    wealth = wealth,
    liquid = liquid,
    house = house,
    debt = debt
  )
}

# -----------------------------------------------------------------------------
# Rent scenario
# -----------------------------------------------------------------------------
run_rent <- function(sim, paths, p) {

  years <- p$mortgage_years

  wealth <- liquid <- numeric(years + 1)

  # Initial conditions
  liquid[1] <- p$initial_capital
  wealth[1] <- liquid[1]

  salary <- p$annual_salary

  # Initial rent
  rent <- p$house_price * p$rental_yield * p$rent_markup

  for (t in 2:(years + 1)) {
    y <- t - 1

    # Salary evolution
    salary <- salary * (1 + paths$salary[y, sim])
    savings <- salary * p$savings_rate

    # Rent indexed to inflation
    rent <- rent * (1 + paths$infl[y, sim])

    # Liquid capital
    liquid[t] <- liquid[t - 1] * (1 + paths$inv[y, sim]) +
      savings -
      rent

    wealth[t] <- liquid[t]
  }

  list(
    scenario = "Rent",
    wealth = wealth,
    liquid = liquid
  )
}

# -----------------------------------------------------------------------------
# Cash / no housing scenario
# -----------------------------------------------------------------------------
run_cash <- function(sim, paths, p) {

  years <- p$mortgage_years

  wealth <- liquid <- numeric(years + 1)

  # Initial conditions
  liquid[1] <- p$initial_capital
  wealth[1] <- liquid[1]

  salary <- p$annual_salary

  for (t in 2:(years + 1)) {
    y <- t - 1

    # Salary evolution
    salary <- salary * (1 + paths$salary[y, sim])
    savings <- salary * p$savings_rate

    # Liquid capital
    liquid[t] <- liquid[t - 1] * (1 + paths$inv[y, sim]) +
      savings

    wealth[t] <- liquid[t]
  }

  list(
    scenario = "Cash",
    wealth = wealth,
    liquid = liquid
  )
}
