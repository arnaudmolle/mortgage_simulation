# ==============================================================================
# BELGIUM HOUSE PURCHASE FINANCIAL MODEL
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
down_payment_pct <- 0.40  # 20% down payment (80% loan-to-value)

# Economic parameters (Belgian market 2024-2025)
inflation_rate <- 0.010  # 2.4% - Current Belgium inflation (Nov 2025)
house_appreciation <- 0.03  # 3% - Expected house price growth 2025
# Calculate nominal house appreciation from real + inflation
house_appreciation_real <- house_appreciation - inflation_rate  # Extract real rate
house_appreciation_nominal <- house_appreciation  # Keep as nominal
salary_increase <- 0.035  # 3.5% - Average salary indexation 2025

real_investment_return <- 0.04  # 4% real return
investment_return <- real_investment_return + inflation_rate
savings_rate <- 0.30
# Ownership costs
owner_yearly_charge <- 6500  # EUR - Property tax + insurance + maintenance + utilities
# Mortgage parameters (Belgian market)
mortgage_rate <- 0.035  # 3% - Average mortgage rate for 10+ year fixed (2025)
mortgage_years <- 30  # Standard Belgian mortgage term
notary_fees_pct <- 0.03  # ~3% notary fees and costs (Wallonia 2025)
# Tax parameters (Belgium)
mortgage_interest_deduction_rate <- 0.45  # 45% - Belgian regional tax benefit
max_deductible_interest <- 2310  # EUR - Annual cap on deductible interest
# Initial capital and income
initial_capital <- 250000  # EUR - Available cash
annual_salary <- 50000  # EUR - Annual gross salary
rent_markup <- 1.15  # Landlord marks up 15% to cover their costs
# Time horizon
years <- mortgage_years

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

cat("=== HOUSE PURCHASE SCENARIOS - BELGIUM ===\n\n")
cat("House Price:", format(house_price, big.mark=","), "EUR\n")
cat("Notary Fees:", format(notary_fees, big.mark=","), "EUR\n")
cat("Down Payment:", down_payment_pct * 100, "% =", format(down_payment, big.mark=","), "EUR\n")
cat("Loan Amount:", format(loan_amount, big.mark=","), "EUR\n")
cat("Monthly Payment:", format(round(monthly_payment, 2), big.mark=","), "EUR\n")
cat("Annual Payment:", format(round(annual_mortgage_payment, 2), big.mark=","), "EUR\n")
cat("\nINCOME & SAVINGS:\n")
cat("Initial Salary:", format(annual_salary, big.mark=","), "EUR\n")
cat("Savings Rate:", savings_rate * 100, "%\n")
cat("Annual Savings:", format(round(annual_salary * savings_rate), big.mark=","), "EUR\n")
cat("Mortgage/Savings Ratio:", round((annual_mortgage_payment / (annual_salary * savings_rate)) * 100, 1), "%\n\n")
# ==============================================================================
# AUTOMATIC SCENARIO SELECTION
# ==============================================================================

min_down_payment_pct <- 0.10  # 10% minimum down payment required by banks
min_down_payment <- house_price * min_down_payment_pct
total_purchase_cost_min <- min_down_payment + notary_fees
total_purchase_cost_cash <- house_price + notary_fees

cat("=== SCENARIO ELIGIBILITY ===\n\n")
cat("Available Capital:", format(initial_capital, big.mark=","), "EUR\n")
cat("Cash Purchase Requires:", format(total_purchase_cost_cash, big.mark=","), "EUR\n")
cat("Minimum Purchase Requires:", format(total_purchase_cost_min, big.mark=","), "EUR\n\n")

can_buy_cash <- initial_capital >= total_purchase_cost_cash
can_buy_mortgage <- initial_capital >= total_purchase_cost_min
can_only_rent <- !can_buy_mortgage

if (can_only_rent) {
  cat("❌ INSUFFICIENT CAPITAL TO PURCHASE\n")
  cat("   You can only RENT\n")
  cat("   Need at least:", format(total_purchase_cost_min, big.mark=","), "EUR\n\n")
  
} else if (can_buy_cash) {
  cat("✓ CASH PURCHASE POSSIBLE\n")
  cat("✓ MORTGAGE PURCHASE POSSIBLE\n")
  cat("→ Will compare both strategies\n\n")
  
} else if (can_buy_mortgage) {
  cat("✓ MORTGAGE PURCHASE POSSIBLE\n")
  cat("❌ Cash purchase not possible (insufficient capital)\n")
  cat("→ Mortgage is only purchase option\n\n")
}
# ==============================================================================
# SCENARIO 1: CASH PURCHASE
# ==============================================================================

# ==============================================================================
# SCENARIO 1: CASH PURCHASE (if possible)
# ==============================================================================

if (can_buy_cash) {
  
  cash_capital <- numeric(years + 1)
  cash_house_value <- numeric(years + 1)
  cash_total_wealth <- numeric(years + 1)
  cash_salary <- numeric(years + 1)
  cash_annual_savings <- numeric(years + 1)
  
  # Initial state: Buy house, invest remaining capital
  cash_capital[1] <- initial_capital - house_price - notary_fees
  cash_house_value[1] <- house_price
  cash_total_wealth[1] <- cash_capital[1] + cash_house_value[1]
  cash_salary[1] <- annual_salary
  
  cat("CASH PURCHASE SCENARIO:\n")
  cat("  House purchased:", format(house_price, big.mark=","), "EUR\n")
  cat("  Notary fees:", format(notary_fees, big.mark=","), "EUR\n")
  cat("  Remaining capital to invest:", format(cash_capital[1], big.mark=","), "EUR\n\n")
  
  # Year-by-year evolution
  for (year in 2:(years + 1)) {
    y <- year - 1
    
    # Salary increases with indexation
    cash_salary[year] <- cash_salary[year-1] * (1 + salary_increase)
    
    # Owner costs increase with inflation
    owner_costs_inflated <- owner_yearly_charge * (1 + inflation_rate)^(y)
    
    # Calculate annual savings (30% of salary)
    cash_annual_savings[year] <- cash_salary[year] * savings_rate
    
    # Net savings after owner costs
    net_savings <- cash_annual_savings[year] - owner_costs_inflated
    
    # Invest remaining capital + net savings
    cash_capital[year] <- cash_capital[year-1] * (1 + investment_return) + net_savings
    
    # House appreciates
    cash_house_value[year] <- cash_house_value[year-1] * (1 + house_appreciation_nominal)
    
    # Total wealth
    cash_total_wealth[year] <- cash_capital[year] + cash_house_value[year]
  }
  
} else {
  # Cannot buy with cash
  cat("Cash purchase not possible - skipping scenario\n\n")

  cash_capital <- rep(NA, years + 1)
  cash_house_value <- rep(NA, years + 1)
  cash_total_wealth <- rep(NA, years + 1)
  cash_salary <- rep(NA, years + 1)
  cash_annual_savings <- rep(NA, years + 1)
}
# ==============================================================================
# SCENARIO 2: MORTGAGE PURCHASE (if possible)
# ==============================================================================

if (can_buy_mortgage) {
  
  # If we can't buy cash, use maximum available as down payment
  # If we can buy cash, use the specified down_payment_pct
  if (can_buy_cash) {
    actual_down_payment <- house_price * down_payment_pct
  } else {
    # Use all available capital minus closing costs as down payment
    actual_down_payment <- initial_capital - notary_fees
    actual_down_payment_pct <- actual_down_payment / house_price
  }
  
  actual_loan_amount <- house_price - actual_down_payment
  
  # Recalculate mortgage payment with actual loan amount
  actual_monthly_payment <- actual_loan_amount * (monthly_rate * (1 + monthly_rate)^n_payments) / 
                           ((1 + monthly_rate)^n_payments - 1)
  actual_annual_mortgage_payment <- actual_monthly_payment * 12
  
  mortgage_capital <- numeric(years + 1)
  mortgage_house_value <- numeric(years + 1)
  mortgage_debt <- numeric(years + 1)
  mortgage_total_wealth <- numeric(years + 1)
  mortgage_equity <- numeric(years + 1)
  mortgage_salary <- numeric(years + 1)
  
  # Initial state: Down payment + notary fees, invest remaining
  mortgage_capital[1] <- initial_capital - actual_down_payment - notary_fees
  mortgage_house_value[1] <- house_price
  mortgage_debt[1] <- actual_loan_amount
  mortgage_equity[1] <- actual_down_payment
  mortgage_total_wealth[1] <- mortgage_capital[1] + mortgage_equity[1]
  mortgage_salary[1] <- annual_salary
  
  cat("MORTGAGE PURCHASE SCENARIO:\n")
  cat("  House price:", format(house_price, big.mark=","), "EUR\n")
  cat("  Down payment:", format(actual_down_payment, big.mark=","), "EUR",
      "(", round(actual_down_payment_pct * 100, 1), "%)\n")
  cat("  Loan amount:", format(actual_loan_amount, big.mark=","), "EUR\n")
  cat("  Remaining capital to invest:", format(mortgage_capital[1], big.mark=","), "EUR\n")
  cat("  Monthly payment:", format(round(actual_monthly_payment, 2), big.mark=","), "EUR\n\n")
  
  cat("  Monthly payment:", format(round(actual_monthly_payment, 2), big.mark=","), "EUR\n\n")
  
  mortgage_annual_savings <- numeric(years + 1)
  mortgage_cash_flow <- numeric(years + 1)
  
  # Year-by-year evolution
  for (year in 2:(years + 1)) {
    y <- year - 1
    
    # Salary increases with indexation
    mortgage_salary[year] <- mortgage_salary[year-1] * (1 + salary_increase)
    
    # Owner costs increase with inflation
    owner_costs_inflated <- owner_yearly_charge * (1 + inflation_rate)^(y)
    
    # Calculate savings available
    savings_before_mortgage <- mortgage_salary[year] * savings_rate
    
    # Pay mortgage from salary (not from investments!)
    if (y <= mortgage_years) {
      # Calculate interest paid this year
      balance_start <- mortgage_debt[year-1]
      interest_this_year <- balance_start * mortgage_rate
      
      # Tax benefit on mortgage interest (capped)
      deductible_interest <- min(interest_this_year, max_deductible_interest)
      tax_benefit <- deductible_interest * mortgage_interest_deduction_rate
      
      # Net housing cost after tax benefit
      net_housing_cost <- actual_annual_mortgage_payment + owner_costs_inflated - tax_benefit
      
      # After housing costs, remaining savings to invest
      mortgage_annual_savings[year] <- savings_before_mortgage - net_housing_cost
      mortgage_cash_flow[year] <- mortgage_annual_savings[year]
      
      # Calculate remaining debt after year's payments
      remaining_payments <- (mortgage_years - y) * 12
      if (remaining_payments > 0) {
        mortgage_debt[year] <- mortgage_debt[year-1] * (1 + monthly_rate)^12 - 
                               (actual_monthly_payment * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        mortgage_debt[year] <- max(0, mortgage_debt[year])
      } else {
        mortgage_debt[year] <- 0
      }
    } else {
      # Mortgage paid off, only owner costs remain
      mortgage_debt[year] <- 0
      net_housing_cost <- owner_costs_inflated
      mortgage_annual_savings[year] <- savings_before_mortgage - net_housing_cost
      mortgage_cash_flow[year] <- mortgage_annual_savings[year]
    }
    
    # Invest capital + add new savings
    mortgage_capital[year] <- mortgage_capital[year-1] * (1 + investment_return) + mortgage_annual_savings[year]
    
    # House appreciates
    mortgage_house_value[year] <- mortgage_house_value[year-1] * (1 + house_appreciation_nominal)
    
    # Calculate equity
    mortgage_equity[year] <- mortgage_house_value[year] - mortgage_debt[year]
    
    # Total wealth
    mortgage_total_wealth[year] <- mortgage_capital[year] + mortgage_equity[year]
  }
  
  
} else {
  # Cannot buy with mortgage
  cat("Mortgage purchase not possible - skipping scenario\n\n")
  cat("Mortgage purchase not possible - skipping scenario\n\n")
  mortgage_capital <- rep(NA, years + 1)
  mortgage_house_value <- rep(NA, years + 1)
  mortgage_debt <- rep(NA, years + 1)
  mortgage_equity <- rep(NA, years + 1)
  mortgage_total_wealth <- rep(NA, years + 1)
  mortgage_salary <- rep(NA, years + 1)
  mortgage_cash_flow <- rep(NA, years + 1)
  mortgage_annual_savings <- rep(NA, years + 1)
}

# ==============================================================================
# SCENARIO 3: RENT + INVEST ALL CAPITAL
# ==============================================================================

# Estimate monthly rent (4.24% gross rental yield in Belgium)
rental_yield <- 0.0424
monthly_rent <- (house_price * rental_yield / 12) * rent_markup
annual_rent <- monthly_rent * 12

rent_capital <- numeric(years + 1)
rent_total_wealth <- numeric(years + 1)
rent_salary <- numeric(years + 1)
rent_annual_savings <- numeric(years + 1)

# Initial state
rent_capital[1] <- initial_capital
rent_total_wealth[1] <- rent_capital[1]
rent_salary[1] <- annual_salary

# Year-by-year evolution
for (year in 2:(years + 1)) {
  y <- year - 1
  
  # Salary increases with indexation
  rent_salary[year] <- rent_salary[year-1] * (1 + salary_increase)
  
  # Calculate savings available
  savings_before_rent <- rent_salary[year] * savings_rate
  
  # Pay rent (increases with inflation) from salary

  annual_rent_adjusted <- annual_rent * (1 + inflation_rate)^(y)
  
  # Remaining savings after rent
  rent_annual_savings[year] <- savings_before_rent - annual_rent_adjusted
  
  # Invest all capital + add new savings
  rent_capital[year] <- rent_capital[year-1] * (1 + investment_return) + rent_annual_savings[year]
  
  # Total wealth (no property ownership)
  rent_total_wealth[year] <- rent_capital[year]
}

# ==============================================================================
# CREATE DATAFRAME FOR PLOTTING
# ==============================================================================

results_df <- data.frame(
  Year = 0:years,
  Cash_Total = cash_total_wealth,
  Cash_House = cash_house_value,
  Cash_Capital = cash_capital,
  Cash_Salary = cash_salary,
  Mortgage_Total = mortgage_total_wealth,
  Mortgage_Equity = mortgage_equity,
  Mortgage_Capital = mortgage_capital,
  Mortgage_Debt = mortgage_debt,
  Mortgage_Salary = mortgage_salary,
  Mortgage_Cash_Flow = if(exists("mortgage_cash_flow")) c(0, mortgage_cash_flow[2:(years+1)]) else rep(NA, years+1),
  Rent_Total = rent_total_wealth,
  Rent_Salary = rent_salary
)

# ==============================================================================
# CALCULATE KEY METRICS
# ==============================================================================

cat("=== WEALTH AFTER", years, "YEARS ===\n\n")

if (can_buy_cash) {
  cat("CASH PURCHASE:\n")
  cat("  Total Wealth:", format(round(cash_total_wealth[years+1]), big.mark=","), "EUR\n")
  cat("  House Value:", format(round(cash_house_value[years+1]), big.mark=","), "EUR\n")
  cat("  Liquid Capital:", format(round(cash_capital[years+1]), big.mark=","), "EUR\n")
  cat("  Final Salary:", format(round(cash_salary[years+1]), big.mark=","), "EUR\n\n")
} else {
  cat("CASH PURCHASE: Not available\n\n")
}

if (can_buy_mortgage) {
  cat("MORTGAGE PURCHASE:\n")
  cat("  Total Wealth:", format(round(mortgage_total_wealth[years+1]), big.mark=","), "EUR\n")
  cat("  House Equity:", format(round(mortgage_equity[years+1]), big.mark=","), "EUR\n")
  cat("  Liquid Capital:", format(round(mortgage_capital[years+1]), big.mark=","), "EUR\n")
  cat("  Remaining Debt:", format(round(mortgage_debt[years+1]), big.mark=","), "EUR\n")
  cat("  Final Salary:", format(round(mortgage_salary[years+1]), big.mark=","), "EUR\n\n")
} else {
  cat("MORTGAGE PURCHASE: Not available\n\n")
}

cat("RENT + INVEST:\n")
cat("  Total Wealth:", format(round(rent_total_wealth[years+1]), big.mark=","), "EUR\n")
cat("  Final Salary:", format(round(rent_salary[years+1]), big.mark=","), "EUR\n\n")

if (can_buy_cash && can_buy_mortgage) {
  wealth_diff <- mortgage_total_wealth[years+1] - cash_total_wealth[years+1]
  cat("Mortgage vs Cash Difference:", format(round(wealth_diff), big.mark=","), "EUR\n")
  cat("Percentage advantage:", round((wealth_diff / cash_total_wealth[years+1]) * 100, 1), "%\n\n")
}
# ==============================================================================
# PLOT 1: TOTAL WEALTH COMPARISON
# ==============================================================================

# Only plot scenarios that are possible
available_scenarios <- c()
if (can_buy_cash) available_scenarios <- c(available_scenarios, "Cash Purchase")
if (can_buy_mortgage) available_scenarios <- c(available_scenarios, "Mortgage Purchase")
available_scenarios <- c(available_scenarios, "Rent + Invest")

wealth_comparison <- results_df %>%
  select(Year, Cash_Total, Mortgage_Total, Rent_Total) %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "Wealth") %>%
  mutate(Scenario = case_when(
    Scenario == "Cash_Total" ~ "Cash Purchase",
    Scenario == "Mortgage_Total" ~ "Mortgage Purchase",
    Scenario == "Rent_Total" ~ "Rent + Invest"
  )) %>%
  filter(!is.na(Wealth), Scenario %in% available_scenarios)

p1 <- ggplot(wealth_comparison, aes(x = Year, y = Wealth, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"), 
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(values = c("Cash Purchase" = "#2E86AB", 
                                 "Mortgage Purchase" = "#A23B72",
                                 "Rent + Invest" = "#F18F01")) +
  scale_linetype_manual(values = c("Cash Purchase" = "solid",
                                    "Mortgage Purchase" = "solid",
                                    "Rent + Invest" = "dashed")) +
  labs(title = "Total Net Wealth Evolution - Belgium House Purchase Scenarios",
       subtitle = paste0("House: €", format(house_price, big.mark=","), 
                        " | Mortgage: ", mortgage_rate*100, "% | Investment: ", 
                        investment_return*100, "%"),
       x = "Years",
       y = "Total Wealth (EUR)",
       color = "Scenario",
       linetype = "Scenario") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p1)

# ==============================================================================
# PLOT 2: WEALTH COMPOSITION (MORTGAGE SCENARIO)
# ==============================================================================
if (can_buy_mortgage) {
wealth_composition <- results_df %>%
  select(Year, Mortgage_Equity, Mortgage_Capital, Mortgage_Debt) %>%
  mutate(Mortgage_Debt = -Mortgage_Debt) %>%  # Make debt negative for stacked area
  pivot_longer(cols = -Year, names_to = "Component", values_to = "Value") %>%
  mutate(Component = case_when(
    Component == "Mortgage_Equity" ~ "House Equity",
    Component == "Mortgage_Capital" ~ "Liquid Capital",
    Component == "Mortgage_Debt" ~ "Debt (Negative)"
  )) %>%
  mutate(Component = factor(Component, levels = c("Liquid Capital", "House Equity", "Debt (Negative)")))

p2 <- ggplot(wealth_composition, aes(x = Year, y = Value, fill = Component)) +
  geom_area(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_fill_manual(values = c("House Equity" = "#06A77D",
                                "Liquid Capital" = "#005B82",
                                "Debt (Negative)" = "#D62828")) +
  labs(title = "Mortgage Scenario - Wealth Composition Over Time",
       subtitle = paste0("Mortgage paid off after ", mortgage_years, " years | Salary invested: ", savings_rate*100, "%"),
       x = "Years",
       y = "Value (EUR)",
       fill = "Component") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p2)
} else {
  cat("Skipping mortgage composition plot - scenario not available\n")
}


# ==============================================================================
# SENSITIVITY ANALYSIS
# ==============================================================================

cat("\n=== SENSITIVITY ANALYSIS ===\n\n")

# Test different investment returns
investment_returns <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08)
sensitivity_results <- data.frame()

for (inv_ret in investment_returns) {
  # Recalculate mortgage scenario with salary
  temp_capital <- initial_capital - down_payment - notary_fees
  temp_equity <- down_payment
  temp_debt <- loan_amount
  temp_salary <- annual_salary
  
  for (year in 2:(years + 1)) {
    y <- year - 1
    temp_salary <- temp_salary * (1 + salary_increase)
    temp_savings <- temp_salary * savings_rate
    
    temp_capital <- temp_capital * (1 + inv_ret)
    
    if (y <= mortgage_years) {
      temp_capital <- temp_capital + (temp_savings - annual_mortgage_payment)
      remaining_payments <- (mortgage_years - y) * 12
      if (remaining_payments > 0) {
        temp_debt <- temp_debt * (1 + monthly_rate)^12 - 
                    (monthly_payment * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        temp_debt <- max(0, temp_debt)
      } else {
        temp_debt <- 0
      }
    } else {
      temp_debt <- 0
      temp_capital <- temp_capital + temp_savings
    }
    
    temp_house <- house_price * (1 + house_appreciation)^(y)
    temp_equity <- temp_house - temp_debt
  }
  
  final_wealth <- temp_capital + temp_equity
  
  sensitivity_results <- rbind(sensitivity_results, data.frame(
    Investment_Return = inv_ret * 100,
    Final_Wealth = final_wealth
  ))
}

cat("Investment Return Sensitivity (", years, " years):\n", sep="")
print(sensitivity_results)
# ==============================================================================
# CRITICAL ANALYSIS: OPTIMAL DOWN PAYMENT FOR YOUR SITUATION
# ==============================================================================

cat("\n=== OPTIMAL DOWN PAYMENT DECISION ===\n\n")
cat("Your situation:\n")
cat("  Available capital: €", format(initial_capital, big.mark=","), "\n")
cat("  House price: €", format(house_price, big.mark=","), "\n")
cat("  Maximum down payment possible:", 
    round(((initial_capital - notary_fees) / house_price) * 100, 1), "%\n\n")

# Create comprehensive grid
down_payment_pcts <- seq(0.10, 0.90, by = 0.05)  # 10% to 90%
investment_returns_test <- seq(0.03, 0.08, by = 0.005)  # 3% to 8%

optimal_grid <- expand.grid(
  Down_Payment_Pct = down_payment_pcts,
  Investment_Return = investment_returns_test
)

optimal_grid$Final_Wealth <- NA
optimal_grid$Final_Liquid <- NA
optimal_grid$Affordable <- NA

cat("Calculating", nrow(optimal_grid), "scenarios...\n")

for (i in 1:nrow(optimal_grid)) {
  dp_pct <- optimal_grid$Down_Payment_Pct[i]
  inv_ret <- optimal_grid$Investment_Return[i]
  
  # Check if this down payment is affordable
  required_cash <- (house_price * dp_pct) + notary_fees
  if (required_cash > initial_capital) {
    optimal_grid$Affordable[i] <- FALSE
    next  # Skip unaffordable scenarios
  }
  optimal_grid$Affordable[i] <- TRUE
  
  # Calculate scenario
  dp_amt <- house_price * dp_pct
  loan_amt <- house_price - dp_amt
  
  if (loan_amt > 0) {
    monthly_p <- loan_amt * (monthly_rate * (1 + monthly_rate)^n_payments) / 
                 ((1 + monthly_rate)^n_payments - 1)
    annual_p <- monthly_p * 12
  } else {
    monthly_p <- 0
    annual_p <- 0
  }
  
  # Simulate
  cap <- initial_capital - dp_amt - notary_fees
  debt <- loan_amt
  sal <- annual_salary
  
  for (year in 2:(years + 1)) {
    y <- year - 1
    
    sal <- sal * (1 + salary_increase)
    sav <- sal * savings_rate
    owner_cost <- owner_yearly_charge * (1 + inflation_rate)^(y)
    
    # Grow capital
    cap <- cap * (1 + inv_ret)
    
    if (y <= mortgage_years && debt > 0) {
      # Calculate tax benefit
      interest_paid <- debt * mortgage_rate
      deductible <- min(interest_paid, max_deductible_interest)
      tax_benefit <- deductible * mortgage_interest_deduction_rate
      
      # Net housing cost
      housing_cost <- annual_p + owner_cost - tax_benefit
      
      # Add net savings
      cap <- cap + (sav - housing_cost)
      
      # Reduce debt
      remaining_p <- (mortgage_years - y) * 12
      if (remaining_p > 0) {
        debt <- debt * (1 + monthly_rate)^12 - 
                (monthly_p * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        debt <- max(0, debt)
      } else {
        debt <- 0
      }
    } else {
      # No mortgage
      debt <- 0
      cap <- cap + (sav - owner_cost)
    }
  }
  
  house_val <- house_price * (1 + house_appreciation_nominal)^years
  equity <- house_val - debt
  total_wealth <- cap + equity
  
  optimal_grid$Final_Wealth[i] <- total_wealth
  optimal_grid$Final_Liquid[i] <- cap
}

# Filter to affordable scenarios only
optimal_grid_affordable <- optimal_grid %>% 
  filter(Affordable == TRUE)

# Find optimal for each investment return
optimal_by_return <- optimal_grid_affordable %>%
  group_by(Investment_Return) %>%
  summarise(
    Optimal_DP = Down_Payment_Pct[which.max(Final_Wealth)],
    Max_Wealth = max(Final_Wealth, na.rm = TRUE)
  )

cat("\nOptimal Down Payment by Investment Return Scenario:\n")
print(optimal_by_return)

# Create heat map
p_optimal <- ggplot(optimal_grid_affordable, 
                    aes(x = Down_Payment_Pct * 100, 
                        y = Investment_Return * 100, 
                        fill = Final_Wealth)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#E63946", 
    mid = "#457B9D", 
    high = "#06A77D",
    midpoint = median(optimal_grid_affordable$Final_Wealth, na.rm = TRUE),
    labels = scales::comma_format(scale = 1e-3, suffix = "k")
  ) +
  geom_contour(aes(z = Final_Wealth), color = "white", alpha = 0.3, bins = 8) +
  geom_point(data = optimal_by_return, 
             aes(x = Optimal_DP * 100, y = Investment_Return * 100),
             color = "yellow", size = 2, shape = 21, fill = "yellow", stroke = 1.5) +
  geom_hline(yintercept = mortgage_rate * 100, 
             linetype = "dashed", color = "white", linewidth = 1) +
  annotate("text", x = 15, y = mortgage_rate * 100 + 0.3,
           label = paste0("Mortgage Rate = ", mortgage_rate * 100, "%"),
           color = "white", fontface = "bold", size = 3.5) +
  labs(
    title = "Optimal Down Payment Strategy",
    subtitle = paste0("Your situation: €", format(initial_capital, big.mark=","), 
                     " available | €", format(house_price, big.mark=","), 
                     " house | ", years, " year horizon\n",
                     "Yellow dots = optimal down payment for each return scenario"),
    x = "Down Payment (%)",
    y = "Investment Return (%)",
    fill = "Final Wealth"
  ) +
  scale_x_continuous(breaks = seq(10, 90, 10)) +
  scale_y_continuous(breaks = seq(3, 8, 0.5)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p_optimal)

# ==============================================================================
# DECISION GUIDE BASED ON YOUR RISK TOLERANCE
# ==============================================================================

cat("\n=== YOUR DECISION GUIDE ===\n\n")

# Find optimal for your actual expected investment return
optimal_for_you <- optimal_grid_affordable %>%
  filter(Investment_Return == investment_return) %>%
  arrange(desc(Final_Wealth)) %>%
  slice(1)

cat("Based on your expected investment return of", investment_return * 100, "%:\n\n")

cat("OPTIMAL STRATEGY:\n")
cat("  Down Payment:", round(optimal_for_you$Down_Payment_Pct * 100, 1), "%\n")
cat("  Down Payment Amount: €", format(round(optimal_for_you$Down_Payment_Pct * house_price), big.mark=","), "\n")
cat("  Expected Wealth (Year", years, "): €", format(round(optimal_for_you$Final_Wealth), big.mark=","), "\n")
cat("  Remaining Liquid: €", format(round(optimal_for_you$Final_Liquid), big.mark=","), "\n\n")

# Compare to alternatives
alternatives <- optimal_grid_affordable %>%
  filter(Investment_Return == investment_return,
         Down_Payment_Pct %in% c(0.20, 0.40, 0.80)) %>%
  arrange(Down_Payment_Pct)

cat("Comparison with common down payment choices:\n")
for (j in 1:nrow(alternatives)) {
  dp <- alternatives$Down_Payment_Pct[j] * 100
  wealth <- alternatives$Final_Wealth[j]
  diff <- wealth - optimal_for_you$Final_Wealth
  pct_diff <- (diff / optimal_for_you$Final_Wealth) * 100
  
  cat(sprintf("  %3.0f%% DP: €%s (%.1f%% %s than optimal)\n",
              dp,
              format(round(wealth), big.mark=","),
              abs(pct_diff),
              ifelse(diff < 0, "worse", "better")))
}

# Sensitivity message
cat("\n")
cat("SENSITIVITY TO MARKET CONDITIONS:\n")
bear_case <- optimal_grid_affordable %>%
  filter(Investment_Return == 0.03) %>%
  arrange(desc(Final_Wealth)) %>%
  slice(1)

bull_case <- optimal_grid_affordable %>%
  filter(Investment_Return == 0.07) %>%
  arrange(desc(Final_Wealth)) %>%
  slice(1)

cat("  If markets underperform (3% return):\n")
cat("    Optimal DP:", round(bear_case$Down_Payment_Pct * 100, 1), 
    "% → Wealth: €", format(round(bear_case$Final_Wealth), big.mark=","), "\n")

cat("  If markets outperform (7% return):\n")
cat("    Optimal DP:", round(bull_case$Down_Payment_Pct * 100, 1), 
    "% → Wealth: €", format(round(bull_case$Final_Wealth), big.mark=","), "\n\n")

# Risk tolerance guidance
if (optimal_for_you$Down_Payment_Pct < 0.30) {
  cat("⚠️  LOW DOWN PAYMENT STRATEGY (High Leverage)\n")
  cat("Pros: Maximizes wealth if investments outperform mortgage\n")
  cat("Cons: Less liquid, higher risk, larger debt\n")
  cat("Best for: High risk tolerance, stable income, confident in investments\n\n")
} else if (optimal_for_you$Down_Payment_Pct > 0.60) {
  cat("🛡️  HIGH DOWN PAYMENT STRATEGY (Low Leverage)\n")
  cat("Pros: Lower debt, more secure, less interest paid\n")
  cat("Cons: Less capital for investments, lower wealth potential\n")
  cat("Best for: Low risk tolerance, value security, conservative\n\n")
} else {
  cat("⚖️  BALANCED DOWN PAYMENT STRATEGY\n")
  cat("Pros: Moderate leverage, balanced risk/return\n")
  cat("Cons: Neither maximizes leverage nor minimizes risk\n")
  cat("Best for: Moderate risk tolerance, balanced approach\n\n")
}

# Export optimal results
write.csv(optimal_grid_affordable, "optimal_down_payment_grid.csv", row.names = FALSE)
write.csv(optimal_by_return, "optimal_by_return.csv", row.names = FALSE)

cat("Detailed results exported to:\n")
cat("  - optimal_down_payment_grid.csv\n")
cat("  - optimal_by_return.csv\n")

# ==============================================================================
# OPTIMAL DOWN PAYMENT ANALYSIS
# ==============================================================================

cat("\n=== OPTIMAL DOWN PAYMENT ANALYSIS ===\n\n")

# Test different down payment percentages
down_payment_range <- seq(0.05, 0.95, by = 0.05)
optimal_dp_results <- data.frame()

for (dp_pct in down_payment_range) {
  dp_amount <- house_price * dp_pct
  loan_amt <- house_price - dp_amount
  
  # Calculate monthly payment
  if (loan_amt > 0) {
    monthly_pmt <- loan_amt * (monthly_rate * (1 + monthly_rate)^n_payments) / 
                   ((1 + monthly_rate)^n_payments - 1)
    annual_pmt <- monthly_pmt * 12
  } else {
    monthly_pmt <- 0
    annual_pmt <- 0
  }
  
  # Simulate wealth accumulation WITH SALARY
  capital <- initial_capital - dp_amount - notary_fees
  debt <- loan_amt
  current_salary <- annual_salary
  
  for (year in 2:(years + 1)) {
    y <- year - 1
    
    # Salary grows
    current_salary <- current_salary * (1 + salary_increase)
    savings_available <- current_salary * savings_rate
    
    # Invest capital
    capital <- capital * (1 + investment_return)
    
    if (y <= mortgage_years && debt > 0) {
      # Pay mortgage from salary savings
      capital <- capital + (savings_available - annual_pmt)
      
      remaining_pmts <- (mortgage_years - y) * 12
      if (remaining_pmts > 0) {
        debt <- debt * (1 + monthly_rate)^12 - 
                (monthly_pmt * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        debt <- max(0, debt)
      } else {
        debt <- 0
      }
    } else {
      # No mortgage, all savings invested
      capital <- capital + savings_available
    }
  }
  
  house_value_final <- house_price * (1 + house_appreciation)^years
  equity_final <- house_value_final - debt
  total_wealth_final <- capital + equity_final
  
  optimal_dp_results <- rbind(optimal_dp_results, data.frame(
    Down_Payment_Pct = dp_pct * 100,
    Down_Payment_Amount = dp_amount,
    Final_Wealth = total_wealth_final,
    Final_Liquid_Capital = capital,
    Final_Equity = equity_final
  ))
}

# Find optimal down payment
optimal_idx <- which.max(optimal_dp_results$Final_Wealth)
optimal_dp_pct <- optimal_dp_results$Down_Payment_Pct[optimal_idx]
optimal_wealth <- optimal_dp_results$Final_Wealth[optimal_idx]

cat("Optimal Down Payment:", optimal_dp_pct, "%\n")
cat("Expected Wealth at Year", years, ":", format(round(optimal_wealth), big.mark=","), "EUR\n\n")

# Plot optimal down payment analysis
p5 <- ggplot(optimal_dp_results, aes(x = Down_Payment_Pct, y = Final_Wealth)) +
  geom_line(color = "#2E86AB", linewidth = 1.2) +
  geom_point(data = optimal_dp_results[optimal_idx, ], 
             aes(x = Down_Payment_Pct, y = Final_Wealth),
             color = "#E63946", size = 4) +
  geom_vline(xintercept = optimal_dp_pct, linetype = "dashed", color = "#E63946", alpha = 0.5) +
  annotate("text", x = optimal_dp_pct, y = optimal_wealth * 0.95, 
           label = paste0("Optimal: ", optimal_dp_pct, "%"), 
           color = "#E63946", size = 4, hjust = -0.1) +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"),
                     breaks = scales::pretty_breaks(n = 8)) +
  labs(title = "Optimal Down Payment Analysis",
       subtitle = paste0("After ", years, " years | Investment return: ", 
                        investment_return*100, "% | Mortgage rate: ", 
                        mortgage_rate*100, "%"),
       x = "Down Payment (%)",
       y = "Final Total Wealth (EUR)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p5)

# ==============================================================================
# BREAK-EVEN ANALYSIS: When does mortgage beat cash?
# ==============================================================================

cat("\n=== BREAK-EVEN ANALYSIS ===\n\n")

if (can_buy_cash && can_buy_mortgage) {
  # Calculate year-by-year comparison
  breakeven_year <- NA
  for (year in 1:years) {
    if (mortgage_total_wealth[year+1] > cash_total_wealth[year+1]) {
      breakeven_year <- year
      break
    }
  }
  
  if (!is.na(breakeven_year)) {
  
  cat("Break-even point: Year", breakeven_year, "\n")
  cat("At this point, mortgage strategy surpasses cash purchase\n")
  wealth_at_breakeven_mortgage <- mortgage_total_wealth[breakeven_year+1]
  wealth_at_breakeven_cash <- cash_total_wealth[breakeven_year+1]
  cat("Mortgage wealth:", format(round(wealth_at_breakeven_mortgage), big.mark=","), "EUR\n")
  cat("Cash wealth:", format(round(wealth_at_breakeven_cash), big.mark=","), "EUR\n\n")
} else {
  cat("Mortgage strategy does not surpass cash purchase within", years, "years\n")
  cat("This suggests: mortgage rate >= investment return\n\n")
}
} else {
  cat("Break-even analysis requires both cash and mortgage scenarios\n\n")
}
# ==============================================================================
# LEVERAGE OPTIMIZATION ANALYSIS
# ==============================================================================

cat("\n=== LEVERAGE OPTIMIZATION: Rate Spread Analysis ===\n\n")

rate_spread <- investment_return - mortgage_rate
cat("Investment Return:", investment_return * 100, "%\n")
cat("Mortgage Rate:", mortgage_rate * 100, "%\n")
cat("Rate Spread:", rate_spread * 100, "%\n\n")

if (rate_spread > 0) {
  cat("POSITIVE LEVERAGE: Investment returns exceed borrowing costs\n")
  cat("Recommendation: Use maximum available leverage (minimize down payment)\n")
  cat("Rationale: Each borrowed euro earns more in investments than it costs in interest\n\n")
} else if (rate_spread < 0) {
  cat("NEGATIVE LEVERAGE: Borrowing costs exceed investment returns\n")
  cat("Recommendation: Minimize or avoid debt (maximize down payment or pay cash)\n")
  cat("Rationale: Each borrowed euro costs more in interest than it can earn\n\n")
} else {
  cat("NEUTRAL LEVERAGE: Investment returns equal borrowing costs\n")
  cat("Recommendation: Indifferent between cash and mortgage\n")
  cat("Consider other factors: liquidity needs, tax implications, risk tolerance\n\n")
}

# ==============================================================================
# 3D SENSITIVITY GRID: Down Payment % vs Rate Spread
# ==============================================================================

cat("\n=== MULTI-PARAMETER SENSITIVITY ANALYSIS ===\n\n")

# Create grid of scenarios
mortgage_rates_test <- seq(0.01, 0.06, by = 0.01)
down_payments_test <- c(0.10, 0.20, 0.40, 0.60, 0.80)
inflation_rates_test <- seq(0.01, 0.05, by = 0.01)

# Scenario 1: Mortgage Rate vs Down Payment
sensitivity_grid_1 <- expand.grid(
  Mortgage_Rate = mortgage_rates_test,
  Down_Payment = down_payments_test
)

sensitivity_grid_1$Final_Wealth <- NA
sensitivity_grid_1$Better_Than_Cash <- NA

for (i in 1:nrow(sensitivity_grid_1)) {
  mort_rate <- sensitivity_grid_1$Mortgage_Rate[i]
  dp_pct <- sensitivity_grid_1$Down_Payment[i]
  
  dp_amt <- house_price * dp_pct
  loan_amt <- house_price - dp_amt
  monthly_r <- mort_rate / 12
  
  if (loan_amt > 0) {
    monthly_p <- loan_amt * (monthly_r * (1 + monthly_r)^n_payments) / 
                 ((1 + monthly_r)^n_payments - 1)
    annual_p <- monthly_p * 12
  } else {
    monthly_p <- 0
    annual_p <- 0
  }
  
  cap <- initial_capital - dp_amt - notary_fees
  debt_remaining <- loan_amt
  current_sal <- annual_salary
  
  for (year in 2:(years + 1)) {
    y <- year - 1
    
    # Salary grows
    current_sal <- current_sal * (1 + salary_increase)
    savings_avail <- current_sal * savings_rate
    
    cap <- cap * (1 + investment_return)
    
    if (y <= mortgage_years && debt_remaining > 0) {
      cap <- cap + (savings_avail - annual_p)
      remaining_p <- (mortgage_years - y) * 12
      if (remaining_p > 0) {
        debt_remaining <- debt_remaining * (1 + monthly_r)^12 - 
                         (monthly_p * (((1 + monthly_r)^12 - 1) / monthly_r))
        debt_remaining <- max(0, debt_remaining)
      } else {
        debt_remaining <- 0
      }
    } else {
      cap <- cap + savings_avail
    }
  }
  
  hv_final <- house_price * (1 + house_appreciation)^years
  eq_final <- hv_final - debt_remaining
  total_w <- cap + eq_final
  
  sensitivity_grid_1$Final_Wealth[i] <- total_w
  sensitivity_grid_1$Better_Than_Cash[i] <- ifelse(total_w > cash_total_wealth[years+1], 
                                                    "Mortgage Better", "Cash Better")
}

# Plot heatmap: Mortgage Rate vs Down Payment
sensitivity_grid_1$Down_Payment_Label <- factor(
  paste0(sensitivity_grid_1$Down_Payment * 100, "%"),
  levels = paste0(sort(unique(sensitivity_grid_1$Down_Payment)) * 100, "%")
)

p6 <- ggplot(sensitivity_grid_1, aes(x = Mortgage_Rate * 100, y = Down_Payment_Label, 
                                      fill = Final_Wealth)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::comma(round(Final_Wealth/1000), suffix = "k")), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_gradient2(low = "#E63946", mid = "#457B9D", high = "#06A77D",
                       midpoint = median(sensitivity_grid_1$Final_Wealth),
                       labels = scales::comma_format(suffix = " €")) +
  labs(title = "Wealth Sensitivity: Mortgage Rate vs Down Payment",
       subtitle = paste0("After ", years, " years | Investment return: ", 
                        investment_return*100, "%"),
       x = "Mortgage Rate (%)",
       y = "Down Payment",
       fill = "Final Wealth") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid = element_blank(),
        legend.position = "right")

print(p6)

# Scenario 2: Investment Return vs Inflation
sensitivity_grid_2 <- expand.grid(
  Investment_Return = seq(0.03, 0.10, by = 0.01),
  Inflation_Rate = inflation_rates_test
)

sensitivity_grid_2$Mortgage_Better <- NA

for (i in 1:nrow(sensitivity_grid_2)) {
  inv_ret <- sensitivity_grid_2$Investment_Return[i]
  infl_rate <- sensitivity_grid_2$Inflation_Rate[i]
  
  # Mortgage scenario with varying investment return and salary
  cap_m <- initial_capital - down_payment - notary_fees
  debt_m <- loan_amount
  sal_m <- annual_salary
  
  for (year in 2:(years + 1)) {
    y <- year - 1
    sal_m <- sal_m * (1 + salary_increase)
    sav_m <- sal_m * savings_rate
    
    cap_m <- cap_m * (1 + inv_ret)
    
    if (y <= mortgage_years) {
      cap_m <- cap_m + (sav_m - annual_mortgage_payment)
      remaining_p <- (mortgage_years - y) * 12
      if (remaining_p > 0) {
        debt_m <- debt_m * (1 + monthly_rate)^12 - 
                 (monthly_payment * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        debt_m <- max(0, debt_m)
      } else {
        debt_m <- 0
      }
    } else {
      cap_m <- cap_m + sav_m
    }
  }
  
  hv_m <- house_price * (1 + house_appreciation)^years
  eq_m <- hv_m - debt_m
  wealth_m <- cap_m + eq_m
  
  # Cash scenario with varying investment return and salary
  cap_c <- initial_capital - house_price - notary_fees
  sal_c <- annual_salary
  for (year in 2:(years + 1)) {
    sal_c <- sal_c * (1 + salary_increase)
    sav_c <- sal_c * savings_rate
    cap_c <- cap_c * (1 + inv_ret) + sav_c
  }
  wealth_c <- cap_c + hv_m
  
  sensitivity_grid_2$Mortgage_Better[i] <- wealth_m > wealth_c
}

# Plot decision boundary
p7 <- ggplot(sensitivity_grid_2, aes(x = Investment_Return * 100, 
                                      y = Inflation_Rate * 100, 
                                      fill = Mortgage_Better)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_abline(intercept = mortgage_rate * 100, slope = 0, 
              linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 8, y = 4.5, 
           label = paste0("Mortgage Rate = ", mortgage_rate * 100, "%"), 
           size = 4, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#06A77D", "FALSE" = "#E63946"),
                    labels = c("TRUE" = "Use Mortgage", "FALSE" = "Pay Cash")) +
  labs(title = "Optimal Strategy: Investment Return vs Inflation",
       subtitle = paste0("When investment return > mortgage rate (", 
                        mortgage_rate*100, "%), use leverage"),
       x = "Investment Return (%)",
       y = "Inflation Rate (%)",
       fill = "Optimal Strategy") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

print(p7)

# ==============================================================================
# DECISION MATRIX OUTPUT
# ==============================================================================

cat("\n=== DECISION MATRIX ===\n\n")
cat("Based on current parameters:\n")
cat("- Investment Return:", investment_return * 100, "%\n")
cat("- Mortgage Rate:", mortgage_rate * 100, "%\n")
cat("- Rate Spread:", (investment_return - mortgage_rate) * 100, "%\n\n")

cat("RECOMMENDATION:\n")
if (investment_return > mortgage_rate) {
  leverage_advantage <- ((1 + investment_return)^years / (1 + mortgage_rate)^years - 1) * 100
  cat("✓ USE MORTGAGE (Positive Leverage)\n")
  cat("  - Minimize down payment (maximize borrowing)\n")
  cat("  - Invest remaining capital for higher returns\n")
  cat("  - Over", years, "years, leverage amplifies wealth by ~", 
      round(leverage_advantage, 1), "%\n\n")
} else if (investment_return < mortgage_rate) {
  cat("✗ AVOID MORTGAGE (Negative Leverage)\n")
  cat("  - Pay cash or maximize down payment\n")
  cat("  - Borrowing costs exceed investment gains\n")
  cat("  - Each borrowed euro loses money over time\n\n")
} else {
  cat("○ NEUTRAL (Indifferent)\n")
  cat("  - Consider non-financial factors:\n")
  cat("    * Liquidity needs\n")
  cat("    * Risk tolerance\n")
  cat("    * Tax implications\n")
  cat("    * Psychological comfort\n\n")
}

cat("KEY INSIGHTS:\n")
cat("1. Optimal down payment with current rates:", optimal_dp_pct, "%\n")
if (!is.na(breakeven_year)) {
  cat("2. Break-even point for mortgage strategy: Year", breakeven_year, "\n")
}
cat("3. Wealth difference after", years, "years:", 
    format(round(wealth_diff), big.mark=","), "EUR\n")
cat("4. This represents a", round((wealth_diff / cash_total_wealth[years+1]) * 100, 1), 
    "% advantage\n\n")

# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

# Save all results
write.csv(results_df, "belgium_house_purchase_results.csv", row.names = FALSE)
write.csv(optimal_dp_results, "optimal_down_payment_analysis.csv", row.names = FALSE)
write.csv(sensitivity_grid_1, "sensitivity_mortgage_rate_vs_down_payment.csv", row.names = FALSE)
write.csv(sensitivity_grid_2, "sensitivity_investment_return_vs_inflation.csv", row.names = FALSE)

cat("Results saved to:\n")
cat("  - belgium_house_purchase_results.csv\n")
cat("  - optimal_down_payment_analysis.csv\n")
cat("  - sensitivity_mortgage_rate_vs_down_payment.csv\n")
cat("  - sensitivity_investment_return_vs_inflation.csv\n")
# ==============================================================================
# OPTIMAL DECISION
# ==============================================================================

cat("\n=== RECOMMENDED STRATEGY ===\n\n")

if (can_only_rent) {
  cat("RECOMMENDATION: RENT\n")
  cat("Reason: Insufficient capital to purchase property\n")
  cat("Build savings until you have at least:", format(total_purchase_cost_min, big.mark=","), "EUR\n\n")
  
} else if (can_buy_cash && can_buy_mortgage) {
  # Compare cash vs mortgage
  rate_spread <- investment_return - mortgage_rate
  
  if (mortgage_total_wealth[years+1] > cash_total_wealth[years+1] && 
      mortgage_total_wealth[years+1] > rent_total_wealth[years+1]) {
    cat("RECOMMENDATION: MORTGAGE\n")
    cat("Reason:\n")
    cat("  - Positive leverage (investment return", investment_return*100, "% > mortgage rate", mortgage_rate*100, "%)\n")
    cat("  - Expected wealth after", years, "years:", format(round(mortgage_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - Advantage over cash:", format(round(mortgage_total_wealth[years+1] - cash_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - Optimal down payment:", round(down_payment_pct * 100, 1), "%\n\n")
    
  } else if (cash_total_wealth[years+1] > rent_total_wealth[years+1]) {
    cat("RECOMMENDATION: CASH PURCHASE\n")
    cat("Reason:\n")
    cat("  - Expected wealth after", years, "years:", format(round(cash_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - Lower risk than mortgage\n")
    cat("  - No debt obligations\n\n")
    
  } else {
    cat("RECOMMENDATION: RENT\n")
    cat("Reason:\n")
    cat("  - Renting provides better returns given current market conditions\n")
    cat("  - Expected wealth after", years, "years:", format(round(rent_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - More flexibility\n\n")
  }
  
} else if (can_buy_mortgage) {
  # Can only afford mortgage
  if (mortgage_total_wealth[years+1] > rent_total_wealth[years+1]) {
    cat("RECOMMENDATION: MORTGAGE\n")
    cat("Reason:\n")
    cat("  - Only purchase option available\n")
    cat("  - Expected wealth after", years, "years:", format(round(mortgage_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - Advantage over renting:", format(round(mortgage_total_wealth[years+1] - rent_total_wealth[years+1]), big.mark=","), "EUR\n\n")
  } else {
    cat("RECOMMENDATION: RENT\n")
    cat("Reason:\n")
    cat("  - Renting provides better returns\n")
    cat("  - Expected wealth after", years, "years:", format(round(rent_total_wealth[years+1]), big.mark=","), "EUR\n")
    cat("  - Save for larger down payment\n\n")
  }
}