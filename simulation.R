# ==============================================================================
# BELGIUM HOUSE PURCHASE FINANCIAL MODEL - REVISED
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

# Time horizon
years <- mortgage_years

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
# SCENARIO 1: CASH PURCHASE (if possible)
# ==============================================================================

if (can_buy_cash) {
  cash_capital <- numeric(years + 1)
  cash_house_value <- numeric(years + 1)
  cash_total_wealth <- numeric(years + 1)
  cash_salary <- numeric(years + 1)
  
  # Initial state
  cash_capital[1] <- initial_capital - house_price - notary_fees
  cash_house_value[1] <- house_price
  cash_total_wealth[1] <- cash_capital[1] + cash_house_value[1]
  cash_salary[1] <- annual_salary
  
  # Year-by-year evolution
  for (year in 2:(years + 1)) {
    y <- year - 1
    cash_salary[year] <- cash_salary[year-1] * (1 + salary_increase)
    owner_costs_inflated <- owner_yearly_charge * (1 + inflation_rate)^(y)
    annual_savings <- cash_salary[year] * savings_rate
    net_savings <- annual_savings - owner_costs_inflated
    cash_capital[year] <- cash_capital[year-1] * (1 + investment_return) + net_savings
    cash_house_value[year] <- cash_house_value[year-1] * (1 + house_appreciation_nominal)
    cash_total_wealth[year] <- cash_capital[year] + cash_house_value[year]
  }
} else {
  cash_capital <- rep(NA, years + 1)
  cash_house_value <- rep(NA, years + 1)
  cash_total_wealth <- rep(NA, years + 1)
  cash_salary <- rep(NA, years + 1)
}

# ==============================================================================
# SCENARIO 2: MORTGAGE PURCHASE (if possible)
# ==============================================================================

if (can_buy_mortgage) {
  # Determine actual down payment
  if (can_buy_cash) {
    actual_down_payment <- house_price * down_payment_pct
    actual_down_payment_pct <- down_payment_pct
  } else {
    actual_down_payment <- initial_capital - notary_fees
    actual_down_payment_pct <- actual_down_payment / house_price
  }
  
  actual_loan_amount <- house_price - actual_down_payment
  
  # Recalculate mortgage payment
  actual_monthly_payment <- actual_loan_amount * (monthly_rate * (1 + monthly_rate)^n_payments) / 
                           ((1 + monthly_rate)^n_payments - 1)
  actual_annual_mortgage_payment <- actual_monthly_payment * 12
  
  mortgage_capital <- numeric(years + 1)
  mortgage_house_value <- numeric(years + 1)
  mortgage_debt <- numeric(years + 1)
  mortgage_total_wealth <- numeric(years + 1)
  mortgage_equity <- numeric(years + 1)
  mortgage_salary <- numeric(years + 1)
  
  # Initial state
  mortgage_capital[1] <- initial_capital - actual_down_payment - notary_fees
  mortgage_house_value[1] <- house_price
  mortgage_debt[1] <- actual_loan_amount
  mortgage_equity[1] <- actual_down_payment
  mortgage_total_wealth[1] <- mortgage_capital[1] + mortgage_equity[1]
  mortgage_salary[1] <- annual_salary
  
  # Year-by-year evolution
  for (year in 2:(years + 1)) {
    y <- year - 1
    mortgage_salary[year] <- mortgage_salary[year-1] * (1 + salary_increase)
    owner_costs_inflated <- owner_yearly_charge * (1 + inflation_rate)^(y)
    savings_before_housing <- mortgage_salary[year] * savings_rate
    
    if (y <= mortgage_years) {
      balance_start <- mortgage_debt[year-1]
      interest_this_year <- balance_start * mortgage_rate
      deductible_interest <- min(interest_this_year, max_deductible_interest)
      tax_benefit <- deductible_interest * mortgage_interest_deduction_rate
      net_housing_cost <- actual_annual_mortgage_payment + owner_costs_inflated - tax_benefit
      annual_savings <- savings_before_housing - net_housing_cost
      
      remaining_payments <- (mortgage_years - y) * 12
      if (remaining_payments > 0) {
        mortgage_debt[year] <- mortgage_debt[year-1] * (1 + monthly_rate)^12 - 
                               (actual_monthly_payment * (((1 + monthly_rate)^12 - 1) / monthly_rate))
        mortgage_debt[year] <- max(0, mortgage_debt[year])
      } else {
        mortgage_debt[year] <- 0
      }
    } else {
      mortgage_debt[year] <- 0
      annual_savings <- savings_before_housing - owner_costs_inflated
    }
    
    mortgage_capital[year] <- mortgage_capital[year-1] * (1 + investment_return) + annual_savings
    mortgage_house_value[year] <- mortgage_house_value[year-1] * (1 + house_appreciation_nominal)
    mortgage_equity[year] <- mortgage_house_value[year] - mortgage_debt[year]
    mortgage_total_wealth[year] <- mortgage_capital[year] + mortgage_equity[year]
  }
} else {
  mortgage_capital <- rep(NA, years + 1)
  mortgage_house_value <- rep(NA, years + 1)
  mortgage_debt <- rep(NA, years + 1)
  mortgage_equity <- rep(NA, years + 1)
  mortgage_total_wealth <- rep(NA, years + 1)
  mortgage_salary <- rep(NA, years + 1)
}

# ==============================================================================
# SCENARIO 3: RENT + INVEST ALL CAPITAL
# ==============================================================================

rental_yield <- 0.0424
monthly_rent <- (house_price * rental_yield / 12) * rent_markup
annual_rent <- monthly_rent * 12

rent_capital <- numeric(years + 1)
rent_total_wealth <- numeric(years + 1)
rent_salary <- numeric(years + 1)

rent_capital[1] <- initial_capital
rent_total_wealth[1] <- rent_capital[1]
rent_salary[1] <- annual_salary

for (year in 2:(years + 1)) {
  y <- year - 1
  rent_salary[year] <- rent_salary[year-1] * (1 + salary_increase)
  savings_before_rent <- rent_salary[year] * savings_rate
  annual_rent_adjusted <- annual_rent * (1 + inflation_rate)^(y)
  annual_savings <- savings_before_rent - annual_rent_adjusted
  rent_capital[year] <- rent_capital[year-1] * (1 + investment_return) + annual_savings
  rent_total_wealth[year] <- rent_capital[year]
}

# ==============================================================================
# PART 2: BASE SCENARIO COMPARISON
# ==============================================================================

cat("\n")
cat("PART 2: BASE SCENARIO COMPARISON (", years, " YEARS)\n", sep="")
cat("--------------------------------------------------------------------------------\n")
cat("Economic Assumptions:\n")
cat("  Investment Return:  ", investment_return * 100, "% (", real_investment_return*100, "% real + ", inflation_rate*100, "% inflation)\n", sep="")
cat("  Mortgage Rate:      ", mortgage_rate * 100, "%\n", sep="")
cat("  House Appreciation: ", house_appreciation_nominal * 100, "%\n", sep="")
cat("  Salary Growth:      ", salary_increase * 100, "%\n", sep="")
cat("  Inflation:          ", inflation_rate * 100, "%\n\n", sep="")

# Create summary table
summary_data <- data.frame(
  Metric = c("Final Total Wealth", "Liquid Capital", "Property Value", "Property Equity", "Remaining Debt"),
  Rent = c(
    format(round(rent_total_wealth[years+1]), big.mark=","),
    format(round(rent_capital[years+1]), big.mark=","),
    "—",
    "—",
    "—"
  ),
  stringsAsFactors = FALSE
)

if (can_buy_cash) {
  summary_data$Cash <- c(
    format(round(cash_total_wealth[years+1]), big.mark=","),
    format(round(cash_capital[years+1]), big.mark=","),
    format(round(cash_house_value[years+1]), big.mark=","),
    format(round(cash_house_value[years+1]), big.mark=","),
    "0"
  )
} else {
  summary_data$Cash <- rep("N/A", 5)
}

if (can_buy_mortgage) {
  summary_data$Mortgage <- c(
    format(round(mortgage_total_wealth[years+1]), big.mark=","),
    format(round(mortgage_capital[years+1]), big.mark=","),
    format(round(mortgage_house_value[years+1]), big.mark=","),
    format(round(mortgage_equity[years+1]), big.mark=","),
    format(round(mortgage_debt[years+1]), big.mark=",")
  )
} else {
  summary_data$Mortgage <- rep("N/A", 5)
}

cat("Summary Results (EUR):\n")
print(summary_data, row.names = FALSE, right = FALSE)

# Ranking
cat("\n")
available_wealth <- c()
available_names <- c()

if (!is.na(rent_total_wealth[years+1])) {
  available_wealth <- c(available_wealth, rent_total_wealth[years+1])
  available_names <- c(available_names, "Rent")
}
if (can_buy_cash && !is.na(cash_total_wealth[years+1])) {
  available_wealth <- c(available_wealth, cash_total_wealth[years+1])
  available_names <- c(available_names, "Cash")
}
if (can_buy_mortgage && !is.na(mortgage_total_wealth[years+1])) {
  available_wealth <- c(available_wealth, mortgage_total_wealth[years+1])
  available_names <- c(available_names, "Mortgage")
}

ranking_order <- order(available_wealth, decreasing = TRUE)
cat("Wealth Ranking:\n")
for (i in 1:length(ranking_order)) {
  idx <- ranking_order[i]
  cat("  ", i, ". ", available_names[idx], ": €", 
      format(round(available_wealth[idx]), big.mark=","), " EUR\n", sep="")
}

if (can_buy_cash && can_buy_mortgage) {
  wealth_diff <- mortgage_total_wealth[years+1] - cash_total_wealth[years+1]
  cat("\nMortgage vs Cash Difference: ", format(round(wealth_diff), big.mark=","), 
      " EUR (", round((wealth_diff / cash_total_wealth[years+1]) * 100, 1), "%)\n", sep="")
}

# ==============================================================================
# CREATE DATAFRAME FOR PLOTTING
# ==============================================================================

results_df <- data.frame(
  Year = 0:years,
  Cash_Total = cash_total_wealth,
  Cash_House = cash_house_value,
  Cash_Capital = cash_capital,
  Mortgage_Total = mortgage_total_wealth,
  Mortgage_Equity = mortgage_equity,
  Mortgage_Capital = mortgage_capital,
  Mortgage_Debt = mortgage_debt,
  Rent_Total = rent_total_wealth,
  Rent_Capital = rent_capital
)

# ==============================================================================
# PLOT 1: TOTAL WEALTH COMPARISON
# ==============================================================================

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
                        round(investment_return*100, 1), "%"),
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
    mutate(Mortgage_Debt = -Mortgage_Debt) %>%
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
}

# ==============================================================================
# PLOT 3: LIQUIDITY COMPARISON
# ==============================================================================

liquidity_comparison <- results_df %>%
  select(Year, Cash_Capital, Mortgage_Capital, Rent_Capital) %>%
  pivot_longer(cols = -Year, names_to = "Scenario", values_to = "Liquidity") %>%
  mutate(Scenario = case_when(
    Scenario == "Cash_Capital" ~ "Cash Purchase",
    Scenario == "Mortgage_Capital" ~ "Mortgage Purchase",
    Scenario == "Rent_Capital" ~ "Rent + Invest"
  )) %>%
  filter(!is.na(Liquidity), Scenario %in% available_scenarios)



# ==============================================================================
# PART 3: OPTIMAL DOWN PAYMENT ANALYSIS
# ==============================================================================

cat("\n\n")
cat("PART 3: OPTIMAL DOWN PAYMENT ANALYSIS\n")
cat("--------------------------------------------------------------------------------\n")

if (!can_buy_mortgage) {
  cat("Cannot perform down payment analysis - mortgage not available with current capital\n")
} else {
  
  # Create comprehensive grid
  down_payment_pcts <- seq(0.10, min(0.95, (initial_capital - notary_fees) / house_price), by = 0.05)
  investment_returns_test <- seq(0.02, 0.10, by = 0.005)
  
  optimal_grid <- expand.grid(
    Down_Payment_Pct = down_payment_pcts,
    Investment_Return = investment_returns_test
  )
  
  optimal_grid$Final_Wealth <- NA
  optimal_grid$Final_Liquid <- NA
  optimal_grid$Affordable <- TRUE
  
  cat("Calculating optimal down payment scenarios...\n")
  
  for (i in 1:nrow(optimal_grid)) {
    dp_pct <- optimal_grid$Down_Payment_Pct[i]
    inv_ret <- optimal_grid$Investment_Return[i]
    
    required_cash <- (house_price * dp_pct) + notary_fees
    if (required_cash > initial_capital) {
      optimal_grid$Affordable[i] <- FALSE
      next
    }
    
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
    
    cap <- initial_capital - dp_amt - notary_fees
    debt <- loan_amt
    sal <- annual_salary
    
    for (year in 2:(years + 1)) {
      y <- year - 1
      sal <- sal * (1 + salary_increase)
      sav <- sal * savings_rate
      owner_cost <- owner_yearly_charge * (1 + inflation_rate)^(y)
      
      cap <- cap * (1 + inv_ret)
      
      if (y <= mortgage_years && debt > 0) {
        interest_paid <- debt * mortgage_rate
        deductible <- min(interest_paid, max_deductible_interest)
        tax_benefit <- deductible * mortgage_interest_deduction_rate
        housing_cost <- annual_p + owner_cost - tax_benefit
        cap <- cap + (sav - housing_cost)
        
        remaining_p <- (mortgage_years - y) * 12
        if (remaining_p > 0) {
          debt <- debt * (1 + monthly_rate)^12 - 
                  (monthly_p * (((1 + monthly_rate)^12 - 1) / monthly_rate))
          debt <- max(0, debt)
        } else {
          debt <- 0
        }
      } else {
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
  
  optimal_grid_affordable <- optimal_grid %>% filter(Affordable == TRUE)
  
  # Find optimal for each investment return
  optimal_by_return <- optimal_grid_affordable %>%
    group_by(Investment_Return) %>%
    summarise(
      Optimal_DP = Down_Payment_Pct[which.max(Final_Wealth)],
      Max_Wealth = max(Final_Wealth, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Find optimal for your expected return
  optimal_for_you <- optimal_grid_affordable %>%
    filter(abs(Investment_Return - investment_return) < 0.001) %>%
    arrange(desc(Final_Wealth)) %>%
    slice(1)
  
  if (nrow(optimal_for_you) > 0) {
    cat("\nBased on your expected investment return of ", investment_return * 100, "%:\n", sep="")
    cat("  Optimal Down Payment:   ", round(optimal_for_you$Down_Payment_Pct * 100, 1), "%\n", sep="")
    cat("  Down Payment Amount:    €", format(round(optimal_for_you$Down_Payment_Pct * house_price), big.mark=","), "\n", sep="")
    cat("  Expected Wealth (Yr ", years, "): €", format(round(optimal_for_you$Final_Wealth), big.mark=","), "\n", sep="")
    cat("  Remaining Liquid:       €", format(round(optimal_for_you$Final_Liquid), big.mark=","), "\n\n", sep="")
  }
  
  # Create heat map
  p4 <- ggplot(optimal_grid_affordable, 
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
               color = "yellow", size = 2.5, shape = 21, fill = "yellow", stroke = 1.5) +
    geom_hline(yintercept = mortgage_rate * 100, 
               linetype = "dashed", color = "white", linewidth = 1) +
    annotate("text", x = max(optimal_grid_affordable$Down_Payment_Pct * 100) * 0.85, 
             y = mortgage_rate * 100 + 0.4,
             label = paste0("Mortgage Rate = ", mortgage_rate * 100, "%"),
             color = "white", fontface = "bold", size = 3.5) +
    labs(
      title = "Optimal Down Payment Strategy",
      subtitle = paste0("€", format(initial_capital, big.mark=","), 
                       " available | €", format(house_price, big.mark=","), 
                       " house | ", years, " year horizon\n",
                       "Yellow dots = optimal down payment for each return scenario"),
      x = "Down Payment (%)",
      y = "Investment Return (%)",
      fill = "Final Wealth (€)"
    ) +
    scale_x_continuous(breaks = seq(10, 100, 10)) +
    scale_y_continuous(breaks = seq(2, 10, 1)) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  print(p4)
  
  # Sensitivity message
  cat("\nSensitivity to Market Conditions:\n")
  bear_case <- optimal_grid_affordable %>%
    filter(Investment_Return == min(Investment_Return)) %>%
    arrange(desc(Final_Wealth)) %>%
    slice(1)
  
  bull_case <- optimal_grid_affordable %>%
    filter(Investment_Return == max(Investment_Return)) %>%
    arrange(desc(Final_Wealth)) %>%
    slice(1)
  
  cat("  Bear Case (", min(optimal_grid_affordable$Investment_Return)*100, "% return):\n", sep="")
  cat("    Optimal DP: ", round(bear_case$Down_Payment_Pct * 100, 1), 
      "% → Wealth: €", format(round(bear_case$Final_Wealth), big.mark=","), "\n", sep="")
  
  cat("  Bull Case (", max(optimal_grid_affordable$Investment_Return)*100, "% return):\n", sep="")
  cat("    Optimal DP: ", round(bull_case$Down_Payment_Pct * 100, 1), 
      "% → Wealth: €", format(round(bull_case$Final_Wealth), big.mark=","), "\n\n", sep="")
  
  # Risk tolerance guidance
  if (nrow(optimal_for_you) > 0) {
    if (optimal_for_you$Down_Payment_Pct < 0.30) {
      cat("⚠️  LOW DOWN PAYMENT STRATEGY (High Leverage)\n")
      cat("    Pros: Maximizes wealth if investments outperform mortgage\n")
      cat("    Cons: Less liquid, higher risk, larger debt\n")
      cat("    Best for: High risk tolerance, stable income, confident in investments\n\n")
    } else if (optimal_for_you$Down_Payment_Pct > 0.60) {
      cat("🛡️  HIGH DOWN PAYMENT STRATEGY (Low Leverage)\n")
      cat("    Pros: Lower debt, more secure, less interest paid\n")
      cat("    Cons: Less capital for investments, lower wealth potential\n")
      cat("    Best for: Low risk tolerance, value security, conservative\n\n")
    } else {
      cat("⚖️  BALANCED DOWN PAYMENT STRATEGY\n")
      cat("    Pros: Moderate leverage, balanced risk/return\n")
      cat("    Cons: Neither maximizes leverage nor minimizes risk\n")
      cat("    Best for: Moderate risk tolerance, balanced approach\n\n")
    }
  }
}

# ==============================================================================
# PART 4: BREAK-EVEN ANALYSIS
# ==============================================================================

cat("\n")
cat("PART 4: BREAK-EVEN ANALYSIS\n")
cat("--------------------------------------------------------------------------------\n")

if (can_buy_cash && can_buy_mortgage) {
  breakeven_year <- NA
  for (year in 1:years) {
    if (mortgage_total_wealth[year+1] > cash_total_wealth[year+1]) {
      breakeven_year <- year
      break
    }
  }
  
  if (!is.na(breakeven_year)) {
    cat("Mortgage vs Cash Break-even: Year ", breakeven_year, "\n", sep="")
    cat("  At this point, mortgage strategy surpasses cash purchase\n")
    cat("  Mortgage wealth: €", format(round(mortgage_total_wealth[breakeven_year+1]), big.mark=","), "\n", sep="")
    cat("  Cash wealth:     €", format(round(cash_total_wealth[breakeven_year+1]), big.mark=","), "\n\n", sep="")
  } else {
    cat("Mortgage strategy does not surpass cash purchase within ", years, " years\n", sep="")
    cat("This suggests: mortgage rate >= investment return (negative leverage)\n\n")
  }
} else if (can_buy_mortgage) {
  # Check mortgage vs rent
  breakeven_year <- NA
  for (year in 1:years) {
    if (mortgage_total_wealth[year+1] > rent_total_wealth[year+1]) {
      breakeven_year <- year
      break
    }
  }
  
  if (!is.na(breakeven_year)) {
    cat("Mortgage vs Rent Break-even: Year ", breakeven_year, "\n", sep="")
    cat("  At this point, mortgage strategy surpasses renting\n\n")
  } else {
    cat("Mortgage does not surpass renting within ", years, " years\n\n", sep="")
  }
}

# Leverage analysis
rate_spread <- investment_return - mortgage_rate
cat("Leverage Analysis:\n")
cat("  Investment Return:  ", round(investment_return * 100, 2), "%\n", sep="")
cat("  Mortgage Rate:      ", round(mortgage_rate * 100, 2), "%\n", sep="")
cat("  Rate Spread:        ", round(rate_spread * 100, 2), "%\n\n", sep="")

if (rate_spread > 0.005) {
  cat("✓ POSITIVE LEVERAGE: Investment returns exceed borrowing costs\n")
  cat("  Each borrowed euro earns more than it costs\n")
  cat("  Recommendation: Minimize down payment to maximize leverage\n\n")
} else if (rate_spread < -0.005) {
  cat("✗ NEGATIVE LEVERAGE: Borrowing costs exceed investment returns\n")
  cat("  Each borrowed euro costs more than it can earn\n")
  cat("  Recommendation: Maximize down payment or pay cash\n\n")
} else {
  cat("○ NEUTRAL LEVERAGE: Returns approximately equal costs\n")
  cat("  Consider other factors: liquidity, risk tolerance, flexibility\n\n")
}

# ==============================================================================
# PART 5: FINAL RECOMMENDATION
# ==============================================================================

cat("\n")
cat("PART 5: FINAL RECOMMENDATION\n")
cat("--------------------------------------------------------------------------------\n")

if (can_only_rent) {
  cat("RECOMMENDATION: RENT\n\n")
  cat("Reason: Insufficient capital to purchase property\n")
  cat("Action: Build savings until you have at least €", 
      format(total_purchase_cost_min, big.mark=","), "\n\n", sep="")
  
} else if (can_buy_cash && can_buy_mortgage) {
  # Compare all three
  best_strategy <- available_names[which.max(available_wealth)]
  best_wealth <- max(available_wealth)
  
  cat("RECOMMENDATION: ", toupper(best_strategy), "\n\n", sep="")
  
  if (best_strategy == "Mortgage") {
    cat("Reason:\n")
    cat("  • Positive leverage (investment return ", round(investment_return*100, 1), 
        "% > mortgage ", round(mortgage_rate*100, 1), "%)\n", sep="")
    cat("  • Expected wealth (Year ", years, "): €", 
        format(round(best_wealth), big.mark=","), "\n", sep="")
    if (can_buy_cash) {
      advantage <- best_wealth - cash_total_wealth[years+1]
      cat("  • Advantage over cash: €", format(round(advantage), big.mark=","), 
          " (+", round((advantage/cash_total_wealth[years+1])*100, 1), "%)\n", sep="")
    }
    if (exists("optimal_for_you") && nrow(optimal_for_you) > 0) {
      cat("  • Optimal down payment: ", round(optimal_for_you$Down_Payment_Pct * 100, 1), "%\n", sep="")
    }
    cat("\n")
    
  } else if (best_strategy == "Cash") {
    cat("Reason:\n")
    cat("  • Expected wealth (Year ", years, "): €", 
        format(round(best_wealth), big.mark=","), "\n", sep="")
    cat("  • No debt obligations or interest costs\n")
    cat("  • Lower risk and full ownership immediately\n")
    cat("  • Better than mortgage due to negative/neutral leverage\n\n")
    
  } else if (best_strategy == "Rent") {
    cat("Reason:\n")
    cat("  • Maximum flexibility and liquidity\n")
    cat("  • Expected wealth (Year ", years, "): €", 
        format(round(best_wealth), big.mark=","), "\n", sep="")
    cat("  • Outperforms buying given current market conditions\n")
    cat("  • Can wait for better market opportunities\n\n")
  }
  
} else if (can_buy_mortgage) {
  if (mortgage_total_wealth[years+1] > rent_total_wealth[years+1]) {
    cat("RECOMMENDATION: MORTGAGE\n\n")
    cat("Reason:\n")
    cat("  • Only purchase option available\n")
    cat("  • Expected wealth (Year ", years, "): €", 
        format(round(mortgage_total_wealth[years+1]), big.mark=","), "\n", sep="")
    advantage <- mortgage_total_wealth[years+1] - rent_total_wealth[years+1]
    cat("  • Advantage over renting: €", format(round(advantage), big.mark=","), 
        " (+", round((advantage/rent_total_wealth[years+1])*100, 1), "%)\n\n", sep="")
  } else {
    cat("RECOMMENDATION: RENT\n\n")
    cat("Reason:\n")
    cat("  • Better financial outcome than available mortgage\n")
    cat("  • Expected wealth (Year ", years, "): €", 
        format(round(rent_total_wealth[years+1]), big.mark=","), "\n", sep="")
    cat("  • Save for larger down payment for better terms\n\n")
  }
}

cat("Key Considerations:\n")
cat("  • This analysis assumes constant returns - actual markets fluctuate\n")
cat("  • Mortgage provides forced savings through equity buildup\n")
cat("  • Renting provides maximum flexibility for life changes\n")
cat("  • Tax benefits included for mortgage scenario\n")
cat("  • Consider personal factors: job security, family plans, risk tolerance\n\n")

# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

write.csv(results_df, "belgium_house_purchase_results.csv", row.names = FALSE)
if (exists("optimal_grid_affordable")) {
  write.csv(optimal_grid_affordable, "optimal_down_payment_grid.csv", row.names = FALSE)
}
if (exists("optimal_by_return")) {
  write.csv(optimal_by_return, "optimal_by_return.csv", row.names = FALSE)
}

cat("\n")
cat("================================================================================\n")
cat("Results exported to CSV files in working directory\n")
cat("Analysis complete!\n")
cat("================================================================================\n")