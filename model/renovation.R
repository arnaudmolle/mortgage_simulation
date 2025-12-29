generate_renovation_shocks <- function(params) {

  years <- params$mortgage_years
  n <- params$n_sim

  shocks <- matrix(0, nrow = years + 1, ncol = n)

  if (params$renovation$enabled) {
    # Clamp renovation year range to actual simulation horizon
    min_year <- max(1, params$renovation$year_range[1])
    max_year <- min(years, params$renovation$year_range[2])
    
    # Only apply shocks if there's a valid year range
    if (min_year <= max_year) {
      for (s in 1:n) {
        if (runif(1) < params$renovation$probability) {
          yr <- sample(min_year:max_year, 1)
          shocks[yr + 1, s] <- params$house_price * params$renovation$cost_pct
        }
      }
    }
  }
  shocks
}
