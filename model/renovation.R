generate_renovation_shocks <- function(params) {

  years <- params$mortgage_years
  n <- params$n_sim

  shocks <- matrix(0, nrow = years + 1, ncol = n)

  if (params$renovation$enabled) {
    for (s in 1:n) {
      if (runif(1) < params$renovation$probability) {
        yr <- sample(
          params$renovation$year_range[1]:
            params$renovation$year_range[2], 1
        )
        shocks[yr + 1, s] <- params$house_price * params$renovation$cost_pct
      }
    }
  }
  shocks
}
