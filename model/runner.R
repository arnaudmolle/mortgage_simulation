run_model <- function(params) {

  validate_params(params)

  paths <- generate_paths(params)
  shocks <- generate_renovation_shocks(params)

  results <- vector("list", params$n_sim)

  for (s in 1:params$n_sim) {
    results[[s]] <- list(
      mortgage = run_mortgage(s, paths, shocks, params),
      rent     = run_rent(s, paths, params),
      cash     = run_cash(s, paths, params)
    )
  }

  results
}
