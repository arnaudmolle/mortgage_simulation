optimize_down_payment <- function(params, dp_grid) {

  out <- data.frame()

  # Generate paths once (use provided seed)
  set.seed(params$seed)
  paths <- generate_paths(params)
  shocks <- generate_renovation_shocks(params)

  for (dp in dp_grid) {
    # Run Monte Carlo for this specific down payment
    mort_wealth <- numeric(params$n_sim)
    mort_liquid <- numeric(params$n_sim)

    for (sim in 1:params$n_sim) {
      result <- run_mortgage(sim, paths, shocks, params, dp_pct = dp)
      mort_wealth[sim] <- tail(result$wealth, 1)
      mort_liquid[sim] <- tail(result$liquid, 1)
    }

    out <- rbind(out, data.frame(
      dp = dp,
      median_wealth = median(mort_wealth),
      mean_wealth = mean(mort_wealth),
      p10 = quantile(mort_wealth, 0.10),
      p90 = quantile(mort_wealth, 0.90),
      median_liquid = median(mort_liquid),
      cv = sd(mort_wealth) / mean(mort_wealth)
    ))
  }
  out
}
