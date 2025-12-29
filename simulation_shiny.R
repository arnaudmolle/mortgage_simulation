default_params <- function() {
  list(
    # House
    house_price = 400000,
    notary_fees_pct = 0.03,

    # Mortgage
    mortgage_rate = 0.035,
    mortgage_years = 30,

    # Returns
    inv_mean = 0.04,
    inv_sd = 0.15,
    house_mean = 0.03,
    house_sd = 0.08,
    inflation_mean = 0.024,
    inflation_sd = 0.015,

    # Correlations
    cor_inv_house = -0.1,
    cor_inv_infl = 0.3,
    cor_inv_salary = 0.5,
    cor_house_salary = -0.3,

    # Income
    annual_salary = 50000,
    savings_rate = 0.30,

    # Simulation
    n_sim = 5000,  # ↓ default for Shiny
    seed = 123
  )
}

run_model <- function(params) {
  set.seed(params$seed)

  # 1. Build correlation matrix
  cor_matrix <- matrix(c(
    1.0, params$cor_inv_infl, params$cor_inv_house, params$cor_inv_salary,
    params$cor_inv_infl, 1.0, -0.2, 0.6,
    params$cor_inv_house, -0.2, 1.0, params$cor_house_salary,
    params$cor_inv_salary, 0.6, params$cor_house_salary, 1.0
  ), nrow = 4)

  # 2. Generate paths
  paths <- generate_correlated_returns(
    n_years = params$mortgage_years,
    n_sims = params$n_sim,
    cor_matrix = cor_matrix,
    params = params
  )

  # 3. Run scenarios
  results <- run_all_scenarios(paths, params)

  # 4. Aggregate outputs
  list(
    wealth = summarize_wealth(results),
    liquidity = summarize_liquidity(results),
    dominance = summarize_dominance(results),
    raw = results
  )
}
