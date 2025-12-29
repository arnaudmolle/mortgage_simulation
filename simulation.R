# ==============================================================================
# BELGIUM HOUSE PURCHASE FINANCIAL MODEL - MONTE CARLO WITH OPTIMAL DOWN PAYMENT
# Compares cash purchase vs. mortgage scenarios with Belgian market defaults
# ==============================================================================

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# ==============================================================================
# INPUT PARAMETERS (Based on 2024-2025 Belgian market data)
# ==============================================================================

# House parameters
house_price <- 400000  # EUR - Average Belgium house price
down_payment_pct <- 0.20  # 20% down payment (80% loan-to-value)

# Economic parameters (Belgian market 2024-2025)
inflation_rate <- 0.024  # 2.4% - Current Belgium inflation (Nov 2025)
house_appreciation <- 0.03  # 3% - Expected house price growth 2025
house_appreciation_nominal <- house_appreciation  # Keep as nominal
salary_increase <- 0.035  # 3.5% - Average salary indexation 2025

real_investment_return <- 0.04  # 4% real return
investment_return <- real_investment_return + inflation_rate
savings_rate <- 0.30

# Ownership costs
owner_yearly_charge <- 6500  # EUR - Property tax + insurance + maintenance + utilities

# Mortgage parameters (Belgian market)
mortgage_rate <- 0.035  # 3.5% - Average mortgage rate for 10+ year fixed (2025)
mortgage_years <- 30  # Standard Belgian mortgage term
notary_fees_pct <- 0.03  # ~3% notary fees and costs (Wallonia 2025)

# Tax parameters (Belgium)
mortgage_interest_deduction_rate <- 0.45  # 45% - Belgian regional tax benefit
max_deductible_interest <- 2310  # EUR - Annual cap on deductible interest

# Initial capital and income
initial_capital <- 250000  # EUR - Available cash
annual_salary <- 50000  # EUR - Annual gross salary
rent_markup <- 1.15  # Landlord marks up 15% to cover their costs
# Rental calculation
rental_yield <- 0.0424  # ~4.24% rental yield for Belgium
monthly_rent <- (house_price * rental_yield / 12) * rent_markup
annual_rent <- monthly_rent * 12
# Time horizon
years <- mortgage_years

# ==============================================================================
# MONTE CARLO SIMULATION PARAMETERS
# ==============================================================================

n_simulations <- 10000  # Number of Monte Carlo runs
set.seed(123)  # For reproducibility

# Distribution parameters (annual rates)
params <- list(
  investment_return = list(mean = 0.04, sd = 0.15),  # Real return
  inflation = list(mean = 0.024, sd = 0.015),
  house_appreciation = list(mean = 0.03, sd = 0.08),  # Real appreciation
  salary_growth = list(mean = 0.035, sd = 0.025)
)

# Correlation matrix (economic relationships)
cor_matrix <- matrix(c(
  1.00,  0.30, -0.40,  0.50,  # Investment return
  0.30,  1.00, -0.20,  0.60,  # Inflation
 -0.20, -0.20,  1.00, -0.30,  # House appreciation (inverse with stocks)
  0.50,  0.60, -0.30,  1.00   # Salary growth
), nrow = 4, byrow = TRUE)

# Renovation shock parameters
renovation_shock <- list(
  enabled = TRUE,
  probability = 0.15,  # 15% chance per 30-year period
  cost_pct = 0.15,     # 15% of house value
  year_range = c(10, 25)  # Can occur between years 10-25
)

# ==============================================================================
# CORRELATED RANDOM NUMBER GENERATION
# ==============================================================================

# Cholesky decomposition for correlated normals
chol_matrix <- chol(cor_matrix)

generate_correlated_returns <- function(n_years, n_sims) {
  # Generate uncorrelated standard normals
  uncorrelated <- array(rnorm(4 * n_years * n_sims), 
                        dim = c(4, n_years, n_sims))
  
  # Apply correlation structure
  correlated <- array(0, dim = c(4, n_years, n_sims))
  for (sim in 1:n_sims) {
    for (year in 1:n_years) {
      correlated[, year, sim] <- chol_matrix %*% uncorrelated[, year, sim]
    }
  }
  
  return(correlated)
}

# Generate all random paths
random_paths <- generate_correlated_returns(years, n_simulations)

# Convert to actual returns
generate_scenario_returns <- function(sim_idx) {
  list(
    inv_return = params$investment_return$mean + params$investment_return$sd * random_paths[1, , sim_idx],
    inflation = params$inflation$mean + params$inflation$sd * random_paths[2, , sim_idx],
    house_appr = params$house_appreciation$mean + params$house_appreciation$sd * random_paths[3, , sim_idx],
    salary_growth = params$salary_growth$mean + params$salary_growth$sd * random_paths[4, , sim_idx]
  )
}

# ==============================================================================
# RENOVATION SHOCK SIMULATION
# ==============================================================================

generate_renovation_shocks <- function(n_sims, house_val) {
  shocks <- matrix(0, nrow = years + 1, ncol = n_sims)
  
  if (renovation_shock$enabled) {
    for (sim in 1:n_sims) {
      if (runif(1) < renovation_shock$probability) {
        shock_year <- sample(renovation_shock$year_range[1]:renovation_shock$year_range[2], 1)
        shocks[shock_year + 1, sim] <- house_val * renovation_shock$cost_pct
      }
    }
  }
  
  return(shocks)
}

renovation_shocks <- generate_renovation_shocks(n_simulations, house_price)

# ==============================================================================
# INPUT VALIDATION
# ==============================================================================

if (down_payment_pct < 0.10 || down_payment_pct > 1.0) {
  stop("Down payment must be between 10% and 100%")
}
if (savings_rate > 1.0 || savings_rate < 0) {
  stop("Savings rate must be between 0 and 1")
}
if (initial_capital < 0 || annual_salary <= 0 || house_price <= 0) {
  stop("Capital, salary, and house price must be positive")
}

# ==============================================================================
# CALCULATED VALUES
# ==============================================================================

down_payment <- house_price * down_payment_pct
loan_amount <- house_price - down_payment
notary_fees <- house_price * notary_fees_pct

# Calculate monthly mortgage payment (fixed annuity)
monthly_rate <- mortgage_rate / 12
n_payments <- mortgage_years * 12
monthly_payment <- loan_amount * (monthly_rate * (1 + monthly_rate)^n_payments) / 
                   ((1 + monthly_rate)^n_payments - 1)
annual_mortgage_payment <- monthly_payment * 12

# ==============================================================================
# PART 1: YOUR SITUATION & SCENARIO ELIGIBILITY
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("                    BELGIUM HOUSE PURCHASE ANALYSIS\n")
cat("================================================================================\n\n")

cat("PART 1: YOUR SITUATION\n")
cat("--------------------------------------------------------------------------------\n")
cat("Available Capital:    ", format(initial_capital, big.mark=","), "EUR\n")
cat("Annual Salary:        ", format(annual_salary, big.mark=","), "EUR\n")
cat("Savings Rate:         ", savings_rate * 100, "%\n")
cat("Annual Savings:       ", format(round(annual_salary * savings_rate), big.mark=","), "EUR\n\n")

cat("Target Property:\n")
cat("  House Price:        ", format(house_price, big.mark=","), "EUR\n")
cat("  Notary Fees:        ", format(notary_fees, big.mark=","), "EUR (", notary_fees_pct*100, "%)\n")
cat("  Total Cost:         ", format(house_price + notary_fees, big.mark=","), "EUR\n\n")

min_down_payment_pct <- 0.10
min_down_payment <- house_price * min_down_payment_pct
total_purchase_cost_min <- min_down_payment + notary_fees
total_purchase_cost_cash <- house_price + notary_fees

cat("Scenario Eligibility:\n")
cat("  Cash Purchase:      ")
can_buy_cash <- initial_capital >= total_purchase_cost_cash
if (can_buy_cash) {
  cat("✓ POSSIBLE (need ", format(total_purchase_cost_cash, big.mark=","), " EUR)\n")
} else {
  cat("✗ NOT POSSIBLE (need ", format(total_purchase_cost_cash, big.mark=","), " EUR)\n")
}

cat("  Mortgage Purchase:  ")
can_buy_mortgage <- initial_capital >= total_purchase_cost_min
if (can_buy_mortgage) {
  cat("✓ POSSIBLE (need ", format(total_purchase_cost_min, big.mark=","), " EUR minimum)\n")
} else {
  cat("✗ NOT POSSIBLE (need ", format(total_purchase_cost_min, big.mark=","), " EUR minimum)\n")
}

cat("  Rent:               ✓ ALWAYS POSSIBLE\n\n")

can_only_rent <- !can_buy_mortgage

# ==============================================================================
# MONTE CARLO SCENARIO SIMULATIONS
# ==============================================================================

run_scenario <- function(scenario_type, sim_idx, returns, custom_dp_pct = NULL) {
  # Extract returns for this simulation
  inv_ret <- returns$inv_return
  infl <- returns$inflation
  house_apr <- returns$house_appr
  sal_growth <- returns$salary_growth
  
  wealth <- numeric(years + 1)
  liquid <- numeric(years + 1)
  house_val <- numeric(years + 1)
  debt <- numeric(years + 1)
  
  if (scenario_type == "cash" && can_buy_cash) {
    liquid[1] <- initial_capital - house_price - notary_fees
    house_val[1] <- house_price
    wealth[1] <- liquid[1] + house_val[1]
    debt[1] <- 0
    sal <- annual_salary
    
    for (year in 2:(years + 1)) {
      y <- year - 1
      sal <- sal * (1 + sal_growth[y])
      
      # Apply renovation shock
      renovation_cost <- renovation_shocks[year, sim_idx]
      
      owner_costs <- owner_yearly_charge * prod(1 + infl[1:y])
      savings <- sal * savings_rate
      liquid[year] <- liquid[year-1] * (1 + inv_ret[y]) + (savings - owner_costs - renovation_cost)
      house_val[year] <- house_val[year-1] * (1 + house_apr[y])
      debt[year] <- 0
      wealth[year] <- liquid[year] + house_val[year]
    }
    
  } else if (scenario_type == "mortgage" && can_buy_mortgage) {
    # Use custom down payment if provided, otherwise use default logic
    if (!is.null(custom_dp_pct)) {
      actual_dp_pct <- custom_dp_pct
    } else {
      actual_dp_pct <- if(can_buy_cash) down_payment_pct else (initial_capital - notary_fees) / house_price
    }
    
    actual_dp <- house_price * actual_dp_pct
    actual_loan <- house_price - actual_dp
    
    monthly_p <- if(actual_loan > 0) {
      actual_loan * (monthly_rate * (1 + monthly_rate)^n_payments) / 
        ((1 + monthly_rate)^n_payments - 1)
    } else {
      0
    }
    annual_p <- monthly_p * 12
    
    liquid[1] <- initial_capital - actual_dp - notary_fees
    house_val[1] <- house_price
    debt[1] <- actual_loan
    wealth[1] <- liquid[1] + house_val[1] - debt[1]
    sal <- annual_salary
    
    for (year in 2:(years + 1)) {
      y <- year - 1
      sal <- sal * (1 + sal_growth[y])
      
      # Apply renovation shock
      renovation_cost <- renovation_shocks[year, sim_idx]
      
      owner_costs <- owner_yearly_charge * prod(1 + infl[1:y])
      savings <- sal * savings_rate
      
      if (y <= mortgage_years && debt[year-1] > 0) {
        interest_paid <- debt[year-1] * mortgage_rate
        deductible <- min(interest_paid, max_deductible_interest)
        tax_benefit <- deductible * mortgage_interest_deduction_rate
        housing_cost <- annual_p + owner_costs - tax_benefit
        
        liquid[year] <- liquid[year-1] * (1 + inv_ret[y]) + (savings - housing_cost - renovation_cost)
        
        remaining_p <- (mortgage_years - y) * 12
        if (remaining_p > 0) {
          debt[year] <- debt[year-1] * (1 + monthly_rate)^12 - 
                        (monthly_p * (((1 + monthly_rate)^12 - 1) / monthly_rate))
          debt[year] <- max(0, debt[year])
        } else {
          debt[year] <- 0
        }
      } else {
        debt[year] <- 0
        liquid[year] <- liquid[year-1] * (1 + inv_ret[y]) + (savings - owner_costs - renovation_cost)
      }
      
      house_val[year] <- house_val[year-1] * (1 + house_apr[y])
      wealth[year] <- liquid[year] + house_val[year] - debt[year]
    }
    
  } else if (scenario_type == "rent") {
    liquid[1] <- initial_capital
    wealth[1] <- liquid[1]
    house_val[1] <- 0
    debt[1] <- 0
    sal <- annual_salary
    
    for (year in 2:(years + 1)) {
      y <- year - 1
      sal <- sal * (1 + sal_growth[y])
      savings <- sal * savings_rate
      annual_rent_adjusted <- annual_rent * prod(1 + infl[1:y])
      
      liquid[year] <- liquid[year-1] * (1 + inv_ret[y]) + (savings - annual_rent_adjusted)
      wealth[year] <- liquid[year]
      house_val[year] <- 0
      debt[year] <- 0
    }
  }
  
  return(list(wealth = wealth, liquid = liquid, house_val = house_val, debt = debt))
}

# Run all simulations for base scenarios
cat("\nRunning Monte Carlo simulations for base scenarios...\n")
pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)

results_mc <- list(
  cash = array(NA, dim = c(years + 1, n_simulations, 4)),
  mortgage = array(NA, dim = c(years + 1, n_simulations, 4)),
  rent = array(NA, dim = c(years + 1, n_simulations, 4))
)

for (sim in 1:n_simulations) {
  returns <- generate_scenario_returns(sim)
  
  if (can_buy_cash) {
    cash_result <- run_scenario("cash", sim, returns)
    results_mc$cash[, sim, 1] <- cash_result$wealth
    results_mc$cash[, sim, 2] <- cash_result$liquid
    results_mc$cash[, sim, 3] <- cash_result$house_val
    results_mc$cash[, sim, 4] <- cash_result$debt
  }
  
  if (can_buy_mortgage) {
    mort_result <- run_scenario("mortgage", sim, returns)
    results_mc$mortgage[, sim, 1] <- mort_result$wealth
    results_mc$mortgage[, sim, 2] <- mort_result$liquid
    results_mc$mortgage[, sim, 3] <- mort_result$house_val
    results_mc$mortgage[, sim, 4] <- mort_result$debt
  }
  
  rent_result <- run_scenario("rent", sim, returns)
  results_mc$rent[, sim, 1] <- rent_result$wealth
  results_mc$rent[, sim, 2] <- rent_result$liquid
  results_mc$rent[, sim, 3] <- rent_result$house_val
  results_mc$rent[, sim, 4] <- rent_result$debt
  
  setTxtProgressBar(pb, sim)
}
close(pb)

# ==============================================================================
# MONTE CARLO RESULTS ANALYSIS
# ==============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("                    MONTE CARLO SIMULATION RESULTS\n")
cat("                        (", format(n_simulations, big.mark=","), " simulations)\n", sep="")
cat("================================================================================\n\n")

# Calculate statistics
calc_stats <- function(data) {
  if (all(is.na(data))) return(NULL)
  list(
    mean = mean(data, na.rm = TRUE),
    median = median(data, na.rm = TRUE),
    sd = sd(data, na.rm = TRUE),
    p10 = quantile(data, 0.10, na.rm = TRUE),
    p25 = quantile(data, 0.25, na.rm = TRUE),
    p75 = quantile(data, 0.75, na.rm = TRUE),
    p90 = quantile(data, 0.90, na.rm = TRUE),
    min = min(data, na.rm = TRUE),
    max = max(data, na.rm = TRUE)
  )
}

# Final wealth distributions
cat("FINAL WEALTH DISTRIBUTIONS (Year ", years, "):\n", sep="")
cat("--------------------------------------------------------------------------------\n")

print_stats <- function(stats, name) {
  if (is.null(stats)) {
    cat(name, ": N/A\n\n")
    return()
  }
  cat(name, ":\n", sep="")
  cat("  Mean:      €", format(round(stats$mean), big.mark=","), "\n", sep="")
  cat("  Median:    €", format(round(stats$median), big.mark=","), "\n", sep="")
  cat("  Std Dev:   €", format(round(stats$sd), big.mark=","), "\n", sep="")
  cat("  10th %ile: €", format(round(stats$p10), big.mark=","), "\n", sep="")
  cat("  90th %ile: €", format(round(stats$p90), big.mark=","), "\n", sep="")
  cat("  Range:     €", format(round(stats$min), big.mark=","), " - €", 
      format(round(stats$max), big.mark=","), "\n\n", sep="")
}

rent_stats <- calc_stats(results_mc$rent[years+1, , 1])
print_stats(rent_stats, "Rent + Invest")

if (can_buy_cash) {
  cash_stats <- calc_stats(results_mc$cash[years+1, , 1])
  print_stats(cash_stats, "Cash Purchase")
}

if (can_buy_mortgage) {
  mort_stats <- calc_stats(results_mc$mortgage[years+1, , 1])
  print_stats(mort_stats, "Mortgage Purchase (20% down)")
}

# Time-weighted dominance
cat("\nTIME-WEIGHTED DOMINANCE ANALYSIS:\n")
cat("--------------------------------------------------------------------------------\n")
cat("% of time each strategy has highest wealth:\n\n")

dominance <- matrix(0, nrow = years + 1, ncol = 3)
colnames(dominance) <- c("Rent", "Cash", "Mortgage")

for (year in 1:(years + 1)) {
  for (sim in 1:n_simulations) {
    wealth_vec <- c(
      results_mc$rent[year, sim, 1],
      if(can_buy_cash) results_mc$cash[year, sim, 1] else -Inf,
      if(can_buy_mortgage) results_mc$mortgage[year, sim, 1] else -Inf
    )
    winner <- which.max(wealth_vec)
    dominance[year, winner] <- dominance[year, winner] + 1
  }
}

dominance_pct <- dominance / n_simulations * 100

# Print dominance at key years
key_years <- c(1, 6, 11, 16, 21, 26, years+1)
for (yr in key_years) {
  if (yr <= years + 1) {
    cat("Year ", yr-1, ":\n", sep="")
    cat("  Rent:     ", sprintf("%5.1f%%", dominance_pct[yr, 1]), "\n", sep="")
    if (can_buy_cash) cat("  Cash:     ", sprintf("%5.1f%%", dominance_pct[yr, 2]), "\n", sep="")
    if (can_buy_mortgage) cat("  Mortgage: ", sprintf("%5.1f%%", dominance_pct[yr, 3]), "\n", sep="")
    cat("\n")
  }
}

# Probability of outperformance
cat("\nPROBABILITY OF OUTPERFORMANCE (Final Year):\n")
cat("--------------------------------------------------------------------------------\n")

if (can_buy_cash && can_buy_mortgage) {
  mort_beats_cash <- sum(results_mc$mortgage[years+1, , 1] > results_mc$cash[years+1, , 1], na.rm=TRUE) / n_simulations
  cat("Mortgage beats Cash:  ", sprintf("%5.1f%%", mort_beats_cash * 100), "\n", sep="")
}

if (can_buy_mortgage) {
  mort_beats_rent <- sum(results_mc$mortgage[years+1, , 1] > results_mc$rent[years+1, , 1], na.rm=TRUE) / n_simulations
  cat("Mortgage beats Rent:  ", sprintf("%5.1f%%", mort_beats_rent * 100), "\n", sep="")
}

if (can_buy_cash) {
  cash_beats_rent <- sum(results_mc$cash[years+1, , 1] > results_mc$rent[years+1, , 1], na.rm=TRUE) / n_simulations
  cat("Cash beats Rent:      ", sprintf("%5.1f%%", cash_beats_rent * 100), "\n", sep="")
}

# ==============================================================================
# MONTE CARLO VISUALIZATIONS
# ==============================================================================

# Plot 1: Wealth distribution over time (fan chart)
wealth_quantiles <- data.frame(Year = 0:years)

for (scenario in c("rent", "cash", "mortgage")) {
  if ((scenario == "cash" && !can_buy_cash) || (scenario == "mortgage" && !can_buy_mortgage)) next
  
  for (year in 1:(years + 1)) {
    wealth_data <- results_mc[[scenario]][year, , 1]
    quants <- quantile(wealth_data, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
    
    wealth_quantiles[year, paste0(scenario, "_p10")] <- quants[1]
    wealth_quantiles[year, paste0(scenario, "_p25")] <- quants[2]
    wealth_quantiles[year, paste0(scenario, "_median")] <- quants[3]
    wealth_quantiles[year, paste0(scenario, "_p75")] <- quants[4]
    wealth_quantiles[year, paste0(scenario, "_p90")] <- quants[5]
  }
}

# Create fan chart
p_fan <- ggplot(wealth_quantiles, aes(x = Year)) +
  geom_ribbon(aes(ymin = rent_p10, ymax = rent_p90), fill = "#F18F01", alpha = 0.2) +
  geom_ribbon(aes(ymin = rent_p25, ymax = rent_p75), fill = "#F18F01", alpha = 0.3) +
  geom_line(aes(y = rent_median), color = "#F18F01", linewidth = 1.2) +
  labs(title = "Monte Carlo Wealth Projections - All Scenarios",
       subtitle = paste0(format(n_simulations, big.mark=","), " simulations | Shaded areas show 10-90% and 25-75% ranges"),
       x = "Years", y = "Total Wealth (EUR)") +
  scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

if (can_buy_cash) {
  p_fan <- p_fan +
    geom_ribbon(data = wealth_quantiles, aes(ymin = cash_p10, ymax = cash_p90), 
                fill = "#2E86AB", alpha = 0.2) +
    geom_ribbon(data = wealth_quantiles, aes(ymin = cash_p25, ymax = cash_p75), 
                fill = "#2E86AB", alpha = 0.3) +
    geom_line(data = wealth_quantiles, aes(y = cash_median), 
              color = "#2E86AB", linewidth = 1.2)
}

if (can_buy_mortgage) {
  p_fan <- p_fan +
    geom_ribbon(data = wealth_quantiles, aes(ymin = mortgage_p10, ymax = mortgage_p90), 
                fill = "#A23B72", alpha = 0.2) +
    geom_ribbon(data = wealth_quantiles, aes(ymin = mortgage_p25, ymax = mortgage_p75), 
                fill = "#A23B72", alpha = 0.3) +
    geom_line(data = wealth_quantiles, aes(y = mortgage_median), 
              color = "#A23B72", linewidth = 1.2)
}

print(p_fan)

# Plot 2: Final wealth distributions
final_wealth_df <- data.frame(
  Wealth = c(
    results_mc$rent[years+1, , 1],
    if(can_buy_cash) results_mc$cash[years+1, , 1] else NULL,
    if(can_buy_mortgage) results_mc$mortgage[years+1, , 1] else NULL
  ),
  Scenario = c(
    rep("Rent", n_simulations),
    if(can_buy_cash) rep("Cash", n_simulations) else NULL,
    if(can_buy_mortgage) rep("Mortgage", n_simulations) else NULL
  )
)

p_dist <- ggplot(final_wealth_df, aes(x = Wealth, fill = Scenario)) +
  geom_density(alpha = 0.6) +
  geom_vline(data = final_wealth_df %>% group_by(Scenario) %>% summarise(median = median(Wealth)),
             aes(xintercept = median, color = Scenario), linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::comma_format(suffix = " €")) +
  scale_fill_manual(values = c("Cash" = "#2E86AB", "Mortgage" = "#A23B72", "Rent" = "#F18F01")) +
  scale_color_manual(values = c("Cash" = "#2E86AB", "Mortgage" = "#A23B72", "Rent" = "#F18F01")) +
  labs(title = "Final Wealth Distribution (Year 30)",
       subtitle = "Dashed lines show median values",
       x = "Final Wealth (EUR)", y = "Probability Density") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p_dist)

# Plot 3: Time-weighted dominance
dominance_df <- data.frame(
  Year = rep(0:years, 3),
  Probability = c(dominance_pct[, 1], dominance_pct[, 2], dominance_pct[, 3]) / 100,
  Scenario = c(rep("Rent", years+1), rep("Cash", years+1), rep("Mortgage", years+1))
)

if (!can_buy_cash) dominance_df <- dominance_df %>% filter(Scenario != "Cash")
if (!can_buy_mortgage) dominance_df <- dominance_df %>% filter(Scenario != "Mortgage")

p_dom <- ggplot(dominance_df, aes(x = Year, y = Probability, fill = Scenario)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Cash" = "#2E86AB", "Mortgage" = "#A23B72", "Rent" = "#F18F01")) +
  labs(title = "Strategy Dominance Over Time",
       subtitle = "Probability each strategy has highest wealth",
       x = "Years", y = "Probability of Being Best") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p_dom)
# ==============================================================================
# MONTE CARLO-BASED RECOMMENDATION
# ==============================================================================

cat("\n\n")
cat("MONTE CARLO-BASED RECOMMENDATION:\n")
cat("--------------------------------------------------------------------------------\n")

# Determine winner by median
scenarios_list <- list()
if (!all(is.na(results_mc$rent[years+1, , 1]))) {
  scenarios_list$Rent <- median(results_mc$rent[years+1, , 1], na.rm = TRUE)
}
if (can_buy_cash && !all(is.na(results_mc$cash[years+1, , 1]))) {
  scenarios_list$Cash <- median(results_mc$cash[years+1, , 1], na.rm = TRUE)
}
if (can_buy_mortgage && !all(is.na(results_mc$mortgage[years+1, , 1]))) {
  scenarios_list$Mortgage <- median(results_mc$mortgage[years+1, , 1], na.rm = TRUE)
}

best_scenario <- names(which.max(scenarios_list))
best_median <- max(unlist(scenarios_list))

cat("Best Strategy (by median): ", best_scenario, "\n", sep="")
cat("  Median Final Wealth: €", format(round(best_median), big.mark=","), "\n\n", sep="")

cat("Risk-Adjusted Insights:\n")
cat("  • Consider your risk tolerance when choosing\n")
cat("  • Higher variability = higher risk but potentially higher reward\n")
cat("  • Median represents typical outcome, not guaranteed\n")
cat("  • 10th percentile shows downside risk\n")
cat("  • 90th percentile shows upside potential\n\n")

# Risk profiles
cat("Risk Profiles:\n")
for (scen in names(scenarios_list)) {
  scen_lower <- tolower(scen)
  data_vec <- switch(scen_lower,
                     "rent" = results_mc$rent[years+1, , 1],
                     "cash" = results_mc$cash[years+1, , 1],
                     "mortgage" = results_mc$mortgage[years+1, , 1])
  
  cv <- sd(data_vec, na.rm = TRUE) / mean(data_vec, na.rm = TRUE)
  cat("  ", scen, ": CV = ", sprintf("%.2f", cv), " (", 
      ifelse(cv < 0.3, "Low", ifelse(cv < 0.5, "Medium", "High")), " volatility)\n", sep="")
}

cat("\n")
# Complete Belgium House Purchase Model with Optimal Down Payment Analysis
# This is a continuation - add this after the base scenario analysis

# ==============================================================================
# PART 2: OPTIMAL DOWN PAYMENT ANALYSIS
# ==============================================================================

cat("\n\n")
cat("================================================================================\n")
cat("               PART 2: OPTIMAL DOWN PAYMENT ANALYSIS\n")
cat("================================================================================\n\n")

if (!can_buy_mortgage) {
  cat("Cannot perform down payment analysis - insufficient capital for mortgage\n")
} else {
  
  # Determine feasible down payment range
  max_affordable_dp <- (initial_capital - notary_fees) / house_price
  dp_percentages <- seq(0.10, min(0.95, max_affordable_dp), by = 0.05)
  
  cat("Testing ", length(dp_percentages), " down payment scenarios from ", 
      min(dp_percentages)*100, "% to ", max(dp_percentages)*100, "%...\n", sep="")
  cat("This will take approximately ", round(length(dp_percentages) * 0.15, 1), " minutes\n\n", sep="")
  
  # Storage for results
  optimal_results <- data.frame(
    DownPayment_Pct = dp_percentages,
    Median_Wealth = NA,
    Mean_Wealth = NA,
    SD_Wealth = NA,
    P10_Wealth = NA,
    P90_Wealth = NA,
    Median_Liquid = NA,
    CV = NA,
    Prob_Beat_Rent = NA,
    Prob_Beat_Cash = NA
  )
  
  pb2 <- txtProgressBar(min = 0, max = length(dp_percentages), style = 3)
  
  for (i in seq_along(dp_percentages)) {
    dp_test <- dp_percentages[i]
    
    # Run Monte Carlo for this down payment
    wealth_results <- numeric(n_simulations)
    liquid_results <- numeric(n_simulations)
    
    for (sim in 1:n_simulations) {
      returns <- generate_scenario_returns(sim)
      result <- run_scenario("mortgage", sim, returns, custom_dp_pct = dp_test)
      wealth_results[sim] <- result$wealth[years + 1]
      liquid_results[sim] <- result$liquid[years + 1]
    }
    
    # Calculate statistics
    optimal_results$Median_Wealth[i] <- median(wealth_results, na.rm = TRUE)
    optimal_results$Mean_Wealth[i] <- mean(wealth_results, na.rm = TRUE)
    optimal_results$SD_Wealth[i] <- sd(wealth_results, na.rm = TRUE)
    optimal_results$P10_Wealth[i] <- quantile(wealth_results, 0.10, na.rm = TRUE)
    optimal_results$P90_Wealth[i] <- quantile(wealth_results, 0.90, na.rm = TRUE)
    optimal_results$Median_Liquid[i] <- median(liquid_results, na.rm = TRUE)
    optimal_results$CV[i] <- sd(wealth_results, na.rm = TRUE) / mean(wealth_results, na.rm = TRUE)
    
    # Compare to rent
    optimal_results$Prob_Beat_Rent[i] <- sum(wealth_results > results_mc$rent[years+1, , 1], na.rm=TRUE) / n_simulations
    
    # Compare to cash if available
    if (can_buy_cash) {
      optimal_results$Prob_Beat_Cash[i] <- sum(wealth_results > results_mc$cash[years+1, , 1], na.rm=TRUE) / n_simulations
    }
    
    setTxtProgressBar(pb2, i)
  }
  close(pb2)
  
  # Find optimal down payment
  optimal_idx <- which.max(optimal_results$Median_Wealth)
  optimal_dp <- optimal_results$DownPayment_Pct[optimal_idx]
  optimal_wealth <- optimal_results$Median_Wealth[optimal_idx]
  
  cat("\n\n")
  cat("OPTIMAL DOWN PAYMENT RECOMMENDATION:\n")
  cat("--------------------------------------------------------------------------------\n")
  cat("Optimal Down Payment:     ", sprintf("%.1f%%", optimal_dp * 100), "\n", sep="")
  cat("Down Payment Amount:      €", format(round(optimal_dp * house_price), big.mark=","), "\n", sep="")
  cat("Loan Amount:              €", format(round((1-optimal_dp) * house_price), big.mark=","), "\n", sep="")
  cat("Remaining Cash:           €", format(round(initial_capital - optimal_dp * house_price - notary_fees), big.mark=","), "\n\n", sep="")
  
  cat("Expected Outcomes (Year ", years, "):\n", sep="")
  cat("  Median Wealth:          €", format(round(optimal_wealth), big.mark=","), "\n", sep="")
  cat("  Mean Wealth:            €", format(round(optimal_results$Mean_Wealth[optimal_idx]), big.mark=","), "\n", sep="")
  cat("  10th Percentile:        €", format(round(optimal_results$P10_Wealth[optimal_idx]), big.mark=","), "\n", sep="")
  cat("  90th Percentile:        €", format(round(optimal_results$P90_Wealth[optimal_idx]), big.mark=","), "\n", sep="")
  cat("  Median Liquid Capital:  €", format(round(optimal_results$Median_Liquid[optimal_idx]), big.mark=","), "\n", sep="")
  cat("  Coefficient of Variation: ", sprintf("%.3f", optimal_results$CV[optimal_idx]), "\n\n", sep="")
  
  cat("Performance vs Alternatives:\n")
  cat("  Prob. beats Rent:       ", sprintf("%.1f%%", optimal_results$Prob_Beat_Rent[optimal_idx] * 100), "\n", sep="")
  if (can_buy_cash) {
    cat("  Prob. beats Cash:       ", sprintf("%.1f%%", optimal_results$Prob_Beat_Cash[optimal_idx] * 100), "\n", sep="")
  }
  
  # Compare to default 20%
  if (0.20 %in% dp_percentages) {
    default_idx <- which(dp_percentages == 0.20)
    improvement <- optimal_wealth - optimal_results$Median_Wealth[default_idx]
    improvement_pct <- (improvement / optimal_results$Median_Wealth[default_idx]) * 100
    
    cat("\nImprovement vs 20% Down:\n")
    cat("  Additional Wealth:      €", format(round(improvement), big.mark=","), 
        " (+", sprintf("%.1f%%", improvement_pct), ")\n", sep="")
  }
  
  # Risk analysis
  cat("\nRisk Profile:\n")
  if (optimal_dp < 0.25) {
    cat("⚠️  LOW DOWN PAYMENT STRATEGY (High Leverage)\n")
    cat("   • Maximizes investment capital\n")
    cat("   • Higher potential returns but more debt risk\n")
    cat("   • Requires confidence in investment performance\n")
    cat("   • Less liquidity tied up in home equity\n\n")
  } else if (optimal_dp > 0.50) {
    cat("🛡️  HIGH DOWN PAYMENT STRATEGY (Low Leverage)\n")
    cat("   • Minimizes debt and interest costs\n")
    cat("   • More conservative, lower risk\n")
    cat("   • Less exposed to investment market volatility\n")
    cat("   • May forgo investment growth opportunities\n\n")
  } else {
    cat("⚖️  BALANCED DOWN PAYMENT STRATEGY\n")
    cat("   • Moderate leverage and reasonable debt\n")
    cat("   • Balances investment opportunity with security\n")
    cat("   • Middle-ground risk/return profile\n")
    cat("   • Flexible approach to market conditions\n\n")
  }
  
  # ==============================================================================
  # OPTIMAL DOWN PAYMENT VISUALIZATIONS
  # ==============================================================================
  
  # Plot 4: Down Payment vs Median Wealth with Uncertainty
  p_optimal <- ggplot(optimal_results, aes(x = DownPayment_Pct * 100, y = Median_Wealth)) +
    geom_line(color = "#A23B72", linewidth = 1.5) +
    geom_ribbon(aes(ymin = P10_Wealth, ymax = P90_Wealth), alpha = 0.2, fill = "#A23B72") +
    geom_point(data = optimal_results[optimal_idx, ], 
               aes(x = DownPayment_Pct * 100, y = Median_Wealth),
               color = "#06A77D", size = 5, shape = 21, fill = "#06A77D", stroke = 2) +
    geom_vline(xintercept = optimal_dp * 100, linetype = "dashed", color = "#06A77D", linewidth = 1) +
    annotate("text", x = optimal_dp * 100, y = max(optimal_results$P90_Wealth) * 0.98,
             label = paste0("Optimal: ", sprintf("%.0f%%", optimal_dp * 100)),
             color = "#06A77D", fontface = "bold", hjust = -0.1, size = 4.5) +
    scale_y_continuous(labels = scales::comma_format(suffix = " €"),
                       breaks = scales::pretty_breaks(n = 8)) +
    scale_x_continuous(breaks = seq(10, 100, 10)) +
    labs(
      title = "Optimal Down Payment Analysis",
      subtitle = paste0(format(n_simulations, big.mark=","), " simulations per scenario | Shaded area = 10th-90th percentile range"),
      x = "Down Payment (%)",
      y = "Final Wealth - Median (EUR)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      panel.grid.minor = element_blank()
    )
  
  print(p_optimal)
  
  # Plot 5: Risk-Return Trade-off (CV vs Median Wealth)
  p_risk_return <- ggplot(optimal_results, aes(x = CV, y = Median_Wealth)) +
    geom_path(color = "#457B9D", linewidth = 1.2, arrow = arrow(length = unit(0.3, "cm"))) +
    geom_point(aes(color = DownPayment_Pct * 100), size = 3) +
    geom_point(data = optimal_results[optimal_idx, ],
               aes(x = CV, y = Median_Wealth),
               color = "#06A77D", size = 6, shape = 18) +
    scale_color_gradient2(
      low = "#E63946", mid = "#F18F01", high = "#06A77D",
      midpoint = 50,
      name = "Down\nPayment (%)"
    ) +
    scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
    labs(
      title = "Risk-Return Trade-off by Down Payment",
      subtitle = "Lower CV = less risk | Arrow shows increasing down payment direction",
      x = "Coefficient of Variation (Risk)",
      y = "Median Final Wealth (EUR)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "right"
    )
  
  print(p_risk_return)
  

  
  # Plot 7: Probability Heatmap
  prob_data <- optimal_results %>%
    select(DownPayment_Pct, Prob_Beat_Rent, Prob_Beat_Cash) %>%
    pivot_longer(cols = c(Prob_Beat_Rent, Prob_Beat_Cash), 
                 names_to = "Comparison", values_to = "Probability") %>%
    mutate(Comparison = case_when(
      Comparison == "Prob_Beat_Rent" ~ "vs Rent",
      Comparison == "Prob_Beat_Cash" ~ "vs Cash",
      TRUE ~ Comparison
    )) %>%
    filter(!is.na(Probability))
  
  if (nrow(prob_data) > 0) {
    p_probability <- ggplot(prob_data, aes(x = DownPayment_Pct * 100, y = Comparison, fill = Probability)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_vline(xintercept = optimal_dp * 100, linetype = "dashed", color = "white", linewidth = 1.2) +
      scale_fill_gradient2(
        low = "#E63946", mid = "#F18F01", high = "#06A77D",
        midpoint = 0.5,
        labels = scales::percent_format(),
        name = "Win\nProbability"
      ) +
      scale_x_continuous(breaks = seq(10, 100, 10)) +
      labs(
        title = "Probability of Outperformance by Down Payment",
        subtitle = paste0("Green = higher chance of best outcome | Optimal: ", sprintf("%.0f%%", optimal_dp * 100)),
        x = "Down Payment (%)",
        y = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid = element_blank()
      )
    
    print(p_probability)
  }
  
  # Plot 8: Distribution Comparison (Optimal vs 20% Default)
  if (0.20 %in% dp_percentages) {
    # Generate wealth distributions for optimal and 20%
    wealth_optimal <- numeric(n_simulations)
    wealth_20pct <- numeric(n_simulations)
    
    for (sim in 1:n_simulations) {
      returns <- generate_scenario_returns(sim)
      
      result_opt <- run_scenario("mortgage", sim, returns, custom_dp_pct = optimal_dp)
      wealth_optimal[sim] <- result_opt$wealth[years + 1]
      
      result_20 <- run_scenario("mortgage", sim, returns, custom_dp_pct = 0.20)
      wealth_20pct[sim] <- result_20$wealth[years + 1]
    }
    
    comparison_df <- data.frame(
      Wealth = c(wealth_optimal, wealth_20pct),
      Strategy = c(
        rep(paste0("Optimal (", sprintf("%.0f%%", optimal_dp * 100), ")"), n_simulations),
        rep("Default (20%)", n_simulations)
      )
    )
    
    p_comparison <- ggplot(comparison_df, aes(x = Wealth, fill = Strategy)) +
      geom_density(alpha = 0.6, linewidth = 1) +
      geom_vline(data = comparison_df %>% group_by(Strategy) %>% summarise(median = median(Wealth)),
                 aes(xintercept = median, color = Strategy), 
                 linetype = "dashed", linewidth = 1.2) +
      scale_x_continuous(labels = scales::comma_format(suffix = " €")) +
      scale_fill_manual(values = c("#06A77D", "#A23B72")) +
      scale_color_manual(values = c("#06A77D", "#A23B72")) +
      labs(
        title = "Wealth Distribution: Optimal vs Default Down Payment",
        subtitle = "Dashed lines show median outcomes",
        x = "Final Wealth (EUR)",
        y = "Probability Density"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
    print(p_comparison)
  }
  
  # Summary table
  cat("\n\nDOWN PAYMENT COMPARISON TABLE:\n")
  cat("--------------------------------------------------------------------------------\n")
  
  comparison_table <- optimal_results %>%
    filter(DownPayment_Pct %in% c(0.10, 0.20, 0.30, optimal_dp) | 
           abs(DownPayment_Pct - 0.50) < 0.01) %>%
    arrange(DownPayment_Pct) %>%
    mutate(
      DP_Label = paste0(sprintf("%.0f%%", DownPayment_Pct * 100)),
      across(c(Median_Wealth, P10_Wealth, P90_Wealth), ~format(round(.), big.mark=",")),
      CV = sprintf("%.3f", CV),
      Prob_Beat_Rent = sprintf("%.1f%%", Prob_Beat_Rent * 100)
    ) %>%
    select(DP_Label, Median_Wealth, P10_Wealth, P90_Wealth, CV, Prob_Beat_Rent)
  
  colnames(comparison_table) <- c("Down Payment", "Median €", "10th % €", "90th % €", "Risk (CV)", "Beat Rent")
  
  print(comparison_table, row.names = FALSE, right = FALSE)
  
  cat("\n")
  cat("Key Takeaway: The optimal down payment of ", sprintf("%.0f%%", optimal_dp * 100), 
      " maximizes median wealth\n", sep="")
  cat("while balancing risk and maintaining adequate liquidity.\n")
}

cat("\n")
cat("================================================================================\n")
cat("Analysis complete!\n")
cat("================================================================================\n")