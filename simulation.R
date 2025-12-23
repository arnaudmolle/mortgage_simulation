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
house_price <- 320000  # EUR - Average Belgium house price
down_payment_pct <- 0.20  # 20% down payment (80% loan-to-value)

# Economic parameters (Belgian market 2024-2025)
inflation_rate <- 0.024  # 2.4% - Current Belgium inflation (Nov 2025)
house_appreciation <- 0.03  # 3% - Expected house price growth 2025
salary_increase <- 0.035  # 3.5% - Average salary indexation 2025
investment_return <- 0.06  # 6% - Conservative diversified portfolio return
savings_rate <- 0.30
# Mortgage parameters (Belgian market)
mortgage_rate <- 0.03  # 3% - Average mortgage rate for 10+ year fixed (2025)
mortgage_years <- 25  # Standard Belgian mortgage term
notary_fees_pct <- 0.03  # ~3% notary fees and costs (Wallonia 2025)

# Initial capital and income
initial_capital <- 350000  # EUR - Available cash
annual_salary <- 55000  # EUR - Annual gross salary

# Time horizon
years <- 30

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
# SCENARIO 1: CASH PURCHASE
# ==============================================================================

cash_capital <- numeric(years + 1)
cash_house_value <- numeric(years + 1)
cash_total_wealth <- numeric(years + 1)
cash_salary <- numeric(years + 1)
cash_annual_savings <- numeric(years + 1)

# Initial state
cash_capital[1] <- initial_capital - house_price - notary_fees
cash_house_value[1] <- house_price
cash_total_wealth[1] <- cash_capital[1] + cash_house_value[1]
cash_salary[1] <- annual_salary

# Year-by-year evolution
for (year in 2:(years + 1)) {
  y <- year - 1
  
  # Salary increases with indexation
  cash_salary[year] <- cash_salary[year-1] * (1 + salary_increase)
  
  # Calculate annual savings (30% of salary)
  cash_annual_savings[year] <- cash_salary[year] * savings_rate
  
  # Invest remaining capital + annual savings
  cash_capital[year] <- cash_capital[year-1] * (1 + investment_return) + cash_annual_savings[year]
  
  # House appreciates
  cash_house_value[year] <- cash_house_value[year-1] * (1 + house_appreciation)
  
  # Total wealth
  cash_total_wealth[year] <- cash_capital[year] + cash_house_value[year]
}

# ==============================================================================
# SCENARIO 2: MORTGAGE PURCHASE
# ==============================================================================

mortgage_capital <- numeric(years + 1)
mortgage_house_value <- numeric(years + 1)
mortgage_debt <- numeric(years + 1)
mortgage_total_wealth <- numeric(years + 1)
mortgage_equity <- numeric(years + 1)
mortgage_salary <- numeric(years + 1)
mortgage_annual_savings <- numeric(years + 1)
mortgage_cash_flow <- numeric(years + 1)

# Initial state
mortgage_capital[1] <- initial_capital - down_payment - notary_fees
mortgage_house_value[1] <- house_price
mortgage_debt[1] <- loan_amount
mortgage_equity[1] <- down_payment
mortgage_total_wealth[1] <- mortgage_capital[1] + mortgage_equity[1]
mortgage_salary[1] <- annual_salary

# Year-by-year evolution
for (year in 2:(years + 1)) {
  # Calculate year index (0-based for payment calculations)
  y <- year - 1
  
  # Salary increases with indexation
  mortgage_salary[year] <- mortgage_salary[year-1] * (1 + salary_increase)
  
  # Calculate savings available
  savings_before_mortgage <- mortgage_salary[year] * savings_rate
  
  # Pay mortgage from salary (not from investments!)
  if (y <= mortgage_years) {
    # After mortgage payment, remaining savings to invest
    mortgage_annual_savings[year] <- savings_before_mortgage - annual_mortgage_payment
    mortgage_cash_flow[year] <- mortgage_annual_savings[year]
    
    # Calculate remaining debt after year's payments
    remaining_payments <- (mortgage_years - y) * 12
    if (remaining_payments > 0) {
      mortgage_debt[year] <- mortgage_debt[year-1] * (1 + monthly_rate)^12 - 
                             (monthly_payment * (((1 + monthly_rate)^12 - 1) / monthly_rate))
      mortgage_debt[year] <- max(0, mortgage_debt[year])  # Ensure non-negative
    } else {
      mortgage_debt[year] <- 0
    }
  } else {
    # Mortgage paid off, all savings go to investments
    mortgage_debt[year] <- 0
    mortgage_annual_savings[year] <- savings_before_mortgage
    mortgage_cash_flow[year] <- savings_before_mortgage
  }
  
  # Invest capital + add new savings (or subtract if mortgage exceeds savings early on)
  mortgage_capital[year] <- mortgage_capital[year-1] * (1 + investment_return) + mortgage_annual_savings[year]
  
  # House appreciates
  mortgage_house_value[year] <- mortgage_house_value[year-1] * (1 + house_appreciation)
  
  # Calculate equity
  mortgage_equity[year] <- mortgage_house_value[year] - mortgage_debt[year]
  
  # Total wealth
  mortgage_total_wealth[year] <- mortgage_capital[year] + mortgage_equity[year]
}

# ==============================================================================
# SCENARIO 3: RENT + INVEST ALL CAPITAL
# ==============================================================================

# Estimate monthly rent (4.24% gross rental yield in Belgium)
rental_yield <- 0.0424
monthly_rent <- (house_price * rental_yield) / 12
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
  Mortgage_Cash_Flow = c(0, mortgage_cash_flow[2:(years+1)]),
  Rent_Total = rent_total_wealth,
  Rent_Salary = rent_salary
)

# ==============================================================================
# CALCULATE KEY METRICS
# ==============================================================================

cat("=== WEALTH AFTER", years, "YEARS ===\n\n")
cat("CASH PURCHASE:\n")
cat("  Total Wealth:", format(round(cash_total_wealth[years+1]), big.mark=","), "EUR\n")
cat("  House Value:", format(round(cash_house_value[years+1]), big.mark=","), "EUR\n")
cat("  Liquid Capital:", format(round(cash_capital[years+1]), big.mark=","), "EUR\n")
cat("  Final Salary:", format(round(cash_salary[years+1]), big.mark=","), "EUR\n\n")

cat("MORTGAGE PURCHASE:\n")
cat("  Total Wealth:", format(round(mortgage_total_wealth[years+1]), big.mark=","), "EUR\n")
cat("  House Equity:", format(round(mortgage_equity[years+1]), big.mark=","), "EUR\n")
cat("  Liquid Capital:", format(round(mortgage_capital[years+1]), big.mark=","), "EUR\n")
cat("  Remaining Debt:", format(round(mortgage_debt[years+1]), big.mark=","), "EUR\n")
cat("  Final Salary:", format(round(mortgage_salary[years+1]), big.mark=","), "EUR\n\n")

cat("RENT + INVEST:\n")
cat("  Total Wealth:", format(round(rent_total_wealth[years+1]), big.mark=","), "EUR\n")
cat("  Final Salary:", format(round(rent_salary[years+1]), big.mark=","), "EUR\n\n")

wealth_diff <- mortgage_total_wealth[years+1] - cash_total_wealth[years+1]
cat("Mortgage vs Cash Difference:", format(round(wealth_diff), big.mark=","), "EUR\n")
cat("Percentage advantage:", round((wealth_diff / cash_total_wealth[years+1]) * 100, 1), "%\n\n")

# ==============================================================================
# PLOT 1: TOTAL WEALTH COMPARISON
# ==============================================================================

wealth_comparison <- results_df %>%
  select(Year, Cash_Total, Mortgage_Total, Rent_Total) %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "Wealth") %>%
  mutate(Scenario = case_when(
    Scenario == "Cash_Total" ~ "Cash Purchase",
    Scenario == "Mortgage_Total" ~ "Mortgage Purchase",
    Scenario == "Rent_Total" ~ "Rent + Invest"
  ))

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

# ==============================================================================
# PLOT 3: LIQUID CAPITAL COMPARISON
# ==============================================================================

capital_comparison <- results_df %>%
  select(Year, Cash_Capital, Mortgage_Capital, Rent_Total) %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "Capital") %>%
  mutate(Scenario = case_when(
    Scenario == "Cash_Capital" ~ "Cash Purchase",
    Scenario == "Mortgage_Capital" ~ "Mortgage Purchase",
    Scenario == "Rent_Total" ~ "Rent + Invest"
  ))

p3 <- ggplot(capital_comparison, aes(x = Year, y = Capital, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(values = c("Cash Purchase" = "#2E86AB",
                                 "Mortgage Purchase" = "#A23B72",
                                 "Rent + Invest" = "#F18F01")) +
  labs(title = "Liquid Capital Evolution - Available Cash/Investments",
       subtitle = "Amount available for emergencies or other investments",
       x = "Years",
       y = "Liquid Capital (EUR)",
       color = "Scenario") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p3)

# ==============================================================================
# PLOT 4: MORTGAGE PAYMENT BREAKDOWN
# ==============================================================================

# Calculate interest and principal for each year
payment_breakdown <- data.frame(Year = 1:mortgage_years)
payment_breakdown$Interest <- numeric(mortgage_years)
payment_breakdown$Principal <- numeric(mortgage_years)
payment_breakdown$Balance <- numeric(mortgage_years)

balance <- loan_amount
for (i in 1:mortgage_years) {
  annual_interest <- balance * mortgage_rate
  annual_principal <- annual_mortgage_payment - annual_interest
  
  payment_breakdown$Interest[i] <- annual_interest
  payment_breakdown$Principal[i] <- annual_principal
  payment_breakdown$Balance[i] <- balance
  
  balance <- balance - annual_principal
}

payment_long <- payment_breakdown %>%
  select(Year, Interest, Principal) %>%
  pivot_longer(cols = c(Interest, Principal), names_to = "Type", values_to = "Amount")

p4 <- ggplot(payment_long, aes(x = Year, y = Amount, fill = Type)) +
  geom_area(alpha = 0.7) +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"),
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_fill_manual(values = c("Interest" = "#E63946",
                                "Principal" = "#06A77D")) +
  labs(title = paste0("Annual Mortgage Payment Breakdown (", mortgage_years, " years)"),
       subtitle = paste0("Total annual payment: €", format(round(annual_mortgage_payment), big.mark=",")),
       x = "Year",
       y = "Annual Payment (EUR)",
       fill = "Component") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p4)

# ==============================================================================
# PLOT 5: ANNUAL CASH FLOW COMPARISON
# ==============================================================================

# Create cash flow dataframe
cash_flow_df <- data.frame(
  Year = 1:years,
  Mortgage_Flow = mortgage_cash_flow[2:(years+1)],
  Cash_Flow = cash_annual_savings[2:(years+1)],
  Rent_Flow = rent_annual_savings[2:(years+1)]
)

cash_flow_long <- cash_flow_df %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "Cash_Flow") %>%
  mutate(Scenario = case_when(
    Scenario == "Mortgage_Flow" ~ "Mortgage (Savings - Mortgage Payment)",
    Scenario == "Cash_Flow" ~ "Cash Purchase (30% Savings)",
    Scenario == "Rent_Flow" ~ "Rent (Savings - Rent)"
  ))

p5_cashflow <- ggplot(cash_flow_long, aes(x = Year, y = Cash_Flow, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_y_continuous(labels = scales::label_comma(suffix = " €"),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_color_manual(values = c("Cash Purchase (30% Savings)" = "#2E86AB",
                                 "Mortgage (Savings - Mortgage Payment)" = "#A23B72",
                                 "Rent (Savings - Rent)" = "#F18F01")) +
  labs(title = "Annual Cash Flow Available for Investment",
       subtitle = paste0("Salary increases at ", salary_increase*100, "% per year | Savings rate: ", savings_rate*100, "%"),
       x = "Year",
       y = "Annual Investment (EUR)",
       color = "Scenario") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

print(p5_cashflow)

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