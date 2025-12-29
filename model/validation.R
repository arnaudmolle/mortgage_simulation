validate_params <- function(p) {

  if (p$down_payment_pct < 0.10 || p$down_payment_pct > 1)
    stop("Down payment must be between 10% and 100%")

  if (p$savings_rate < 0 || p$savings_rate > 1)
    stop("Savings rate must be between 0 and 1")

  if (p$initial_capital < 0 || p$annual_salary <= 0 || p$house_price <= 0)
    stop("Capital, salary, and house price must be positive")

  if (p$n_sim < 100 || p$n_sim > 100000)
    stop("Number of simulations must be between 100 and 100,000")

  TRUE
}

