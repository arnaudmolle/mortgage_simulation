final_wealth <- function(results) {
  sapply(results, function(x) tail(x$wealth, 1))
}

final_liquidity <- function(results) {
  sapply(results, function(x) tail(x$liquid, 1))
}

risk_table <- function(results) {
  # Extract final wealth for each scenario
  mort_wealth <- sapply(results, function(x) tail(x$mortgage$wealth, 1))
  rent_wealth <- sapply(results, function(x) tail(x$rent$wealth, 1))
  cash_wealth <- sapply(results, function(x) tail(x$cash$wealth, 1))
  
  # Check if cash scenario has valid data
  has_cash <- !all(is.na(cash_wealth)) && length(cash_wealth) > 0
  
  if (has_cash) {
    data.frame(
      Scenario = c("Mortgage", "Rent", "Cash Purchase"),
      Median = c(median(mort_wealth), median(rent_wealth), median(cash_wealth)),
      P10 = c(quantile(mort_wealth, 0.10), quantile(rent_wealth, 0.10), quantile(cash_wealth, 0.10)),
      P90 = c(quantile(mort_wealth, 0.90), quantile(rent_wealth, 0.90), quantile(cash_wealth, 0.90)),
      SD = c(sd(mort_wealth), sd(rent_wealth), sd(cash_wealth)),
      CV = c(sd(mort_wealth) / mean(mort_wealth), sd(rent_wealth) / mean(rent_wealth), sd(cash_wealth) / mean(cash_wealth))
    )
  } else {
    data.frame(
      Scenario = c("Mortgage", "Rent"),
      Median = c(median(mort_wealth), median(rent_wealth)),
      P10 = c(quantile(mort_wealth, 0.10), quantile(rent_wealth, 0.10)),
      P90 = c(quantile(mort_wealth, 0.90), quantile(rent_wealth, 0.90)),
      SD = c(sd(mort_wealth), sd(rent_wealth)),
      CV = c(sd(mort_wealth) / mean(mort_wealth), sd(rent_wealth) / mean(rent_wealth))
    )
  }
}

# Plot wealth trajectories over time
plot_wealth <- function(results, elig = NULL) {
  n_years <- length(results[[1]]$mortgage$wealth)
  n_sims <- length(results)
  
  # Check if cash scenario has valid data
  cash_check <- sapply(results, function(x) tail(x$cash$wealth, 1))
  has_cash <- !is.null(elig) && elig$can_buy_cash && !all(is.na(cash_check)) && length(cash_check) > 0
  
  # Extract quantiles for each scenario over time
  wealth_data <- data.frame()
  
  for (t in 1:n_years) {
    mort_vals <- sapply(results, function(x) x$mortgage$wealth[t])
    rent_vals <- sapply(results, function(x) x$rent$wealth[t])
    
    year <- t - 1
    
    row_data <- data.frame(
      Year = year,
      Scenario = c("Mortgage", "Rent"),
      Median = c(median(mort_vals), median(rent_vals)),
      P25 = c(quantile(mort_vals, 0.25), quantile(rent_vals, 0.25)),
      P75 = c(quantile(mort_vals, 0.75), quantile(rent_vals, 0.75))
    )
    
    if (has_cash) {
      cash_vals <- sapply(results, function(x) x$cash$wealth[t])
      row_data <- rbind(row_data, data.frame(
        Year = year,
        Scenario = "Cash Purchase",
        Median = median(cash_vals),
        P25 = quantile(cash_vals, 0.25),
        P75 = quantile(cash_vals, 0.75)
      ))
    }
    
    wealth_data <- rbind(wealth_data, row_data)
  }
  
  colors <- c("Mortgage" = "#A23B72", "Rent" = "#F18F01")
  if (has_cash) colors["Cash Purchase"] <- "#2E86AB"
  
  ggplot(wealth_data, aes(x = Year, color = Scenario, fill = Scenario)) +
    geom_ribbon(aes(ymin = P25, ymax = P75), alpha = 0.2, color = NA) +
    geom_line(aes(y = Median), linewidth = 1.2) +
    scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Wealth Trajectories Over Time",
      x = "Years",
      y = "Total Wealth (EUR)",
      subtitle = "Lines show median, ribbons show 25th-75th percentile range"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Plot liquidity over time
plot_liquidity <- function(results, elig = NULL) {
  n_years <- length(results[[1]]$mortgage$liquid)
  n_sims <- length(results)
  
  # Extract quantiles for each scenario over time
  liquid_data <- data.frame()
  
  cash_wealth <- sapply(results, function(x) tail(x$cash$wealth, 1))
  has_cash <- !is.null(elig) && elig$can_buy_cash && !all(is.na(cash_wealth)) && length(cash_wealth) > 0
  
  for (t in 1:n_years) {
    mort_vals <- sapply(results, function(x) x$mortgage$liquid[t])
    rent_vals <- sapply(results, function(x) x$rent$liquid[t])
    
    year <- t - 1
    
    liquid_data <- rbind(liquid_data, data.frame(
      Year = year,
      Scenario = c("Mortgage", "Rent"),
      Median = c(median(mort_vals), median(rent_vals)),
      P25 = c(quantile(mort_vals, 0.25), quantile(rent_vals, 0.25)),
      P75 = c(quantile(mort_vals, 0.75), quantile(rent_vals, 0.75))
    ))
    
    if (has_cash) {
      cash_vals <- sapply(results, function(x) x$cash$liquid[t])
      liquid_data <- rbind(liquid_data, data.frame(
        Year = year,
        Scenario = "Cash Purchase",
        Median = median(cash_vals),
        P25 = quantile(cash_vals, 0.25),
        P75 = quantile(cash_vals, 0.75)
      ))
    }
  }
  
  colors <- c("Mortgage" = "#A23B72", "Rent" = "#F18F01")
  if (has_cash) colors["Cash Purchase"] <- "#2E86AB"
  
  ggplot(liquid_data, aes(x = Year, color = Scenario, fill = Scenario)) +
    geom_ribbon(aes(ymin = P25, ymax = P75), alpha = 0.2, color = NA) +
    geom_line(aes(y = Median), linewidth = 1.2) +
    scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Liquid Capital Over Time",
      x = "Years",
      y = "Liquid Assets (EUR)",
      subtitle = "Lines show median, ribbons show 25th-75th percentile range"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Plot risk distribution - density comparison
plot_risk_distribution <- function(results, elig = NULL) {
  mort_wealth <- sapply(results, function(x) tail(x$mortgage$wealth, 1))
  rent_wealth <- sapply(results, function(x) tail(x$rent$wealth, 1))
  cash_wealth <- sapply(results, function(x) tail(x$cash$wealth, 1))
  
  has_cash <- !is.null(elig) && elig$can_buy_cash && !all(is.na(cash_wealth)) && length(cash_wealth) > 0
  
  dist_data <- data.frame(
    Wealth = c(mort_wealth, rent_wealth),
    Scenario = c(rep("Mortgage", length(mort_wealth)), rep("Rent", length(rent_wealth)))
  )
  
  median_data <- data.frame(
    Scenario = c("Mortgage", "Rent"),
    median = c(median(mort_wealth), median(rent_wealth))
  )
  
  if (has_cash) {
    dist_data <- rbind(dist_data, data.frame(
      Wealth = cash_wealth,
      Scenario = rep("Cash Purchase", length(cash_wealth))
    ))
    median_data <- rbind(median_data, data.frame(
      Scenario = "Cash Purchase",
      median = median(cash_wealth)
    ))
  }
  
  colors <- c("Mortgage" = "#A23B72", "Rent" = "#F18F01")
  if (has_cash) colors["Cash Purchase"] <- "#2E86AB"
  
  ggplot(dist_data, aes(x = Wealth, fill = Scenario, color = Scenario)) +
    geom_density(alpha = 0.6, linewidth = 1) +
    geom_vline(data = median_data, aes(xintercept = median, color = Scenario), linetype = "dashed", linewidth = 1.2) +
    scale_x_continuous(labels = scales::comma_format(suffix = " €")) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(
      title = "Final Wealth Distribution (Year 30)",
      subtitle = "Dashed lines show median values",
      x = "Final Wealth (EUR)",
      y = "Probability Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Plot risk boxplot - visual comparison of statistics
plot_risk_boxplot <- function(results, elig = NULL) {
  mort_wealth <- sapply(results, function(x) tail(x$mortgage$wealth, 1))
  rent_wealth <- sapply(results, function(x) tail(x$rent$wealth, 1))
  cash_wealth <- sapply(results, function(x) tail(x$cash$wealth, 1))
  
  has_cash <- !is.null(elig) && elig$can_buy_cash && !all(is.na(cash_wealth)) && length(cash_wealth) > 0
  
  box_data <- data.frame(
    Wealth = c(mort_wealth, rent_wealth),
    Scenario = c(rep("Mortgage", length(mort_wealth)), rep("Rent", length(rent_wealth)))
  )
  
  if (has_cash) {
    box_data <- rbind(box_data, data.frame(
      Wealth = cash_wealth,
      Scenario = rep("Cash Purchase", length(cash_wealth))
    ))
  }
  
  colors <- c("Mortgage" = "#A23B72", "Rent" = "#F18F01")
  if (has_cash) colors["Cash Purchase"] <- "#2E86AB"
  
  ggplot(box_data, aes(x = Scenario, y = Wealth, fill = Scenario)) +
    geom_boxplot(alpha = 0.7, color = "black", linewidth = 1) +
    geom_jitter(width = 0.2, alpha = 0.1, size = 1) +
    scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
    scale_fill_manual(values = colors) +
    labs(
      title = "Final Wealth Distribution by Scenario",
      subtitle = "Box shows IQR, line shows median, dots show individual simulations",
      x = "Scenario",
      y = "Final Wealth (EUR)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "none",
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 11, face = "bold")
    )
}

