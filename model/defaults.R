default_params <- function() {
  list(
    # House
    house_price = 400000,
    notary_fees_pct = 0.03,
    owner_yearly_charge = 6500,
    rental_yield = 0.0424,
    rent_markup = 1.15,

    # Mortgage
    mortgage_rate = 0.035,
    mortgage_years = 30,
    max_deductible_interest = 2310,
    mortgage_interest_deduction_rate = 0.45,

    # Income
    annual_salary = 50000,
    savings_rate = 0.30,
    initial_capital = 250000,

    # Returns
    inv_mean = 0.04,
    inv_sd = 0.15,
    house_mean = 0.03,
    house_sd = 0.08,
    inflation_mean = 0.024,
    inflation_sd = 0.015,
    salary_mean = 0.035,
    salary_sd = 0.025,

    # Correlations
    cor_inv_house = -0.1,
    cor_inv_infl = 0.3,
    cor_inv_salary = 0.5,
    cor_house_salary = -0.3,

    # Renovation shocks
    renovation = list(
      enabled = TRUE,
      probability = 0.15,
      cost_pct = 0.15,
      year_range = c(10, 25)
    ),

    # Down payment
    down_payment_pct = 0.20,

    # Simulation
    n_sim = 5000,
    seed = 123
  )
}
