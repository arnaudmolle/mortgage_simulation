# ==============================================================================
# BELGIUM REAL ESTATE FINANCIAL OPTIMIZER
# Comparison: Cash vs. Mortgage vs. Rent
# Optimization: Down Payment & Investment Return Sensitivity
# ==============================================================================

# 1. SETUP & LIBRARIES ---------------------------------------------------------
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")

library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# 2. INPUT PARAMETERS (THE "SPECIFIC CASE") ------------------------------------

# --- Economic Environment ---
inflation_rate    <- 0.024   # 2.4% Belgium Inflation
house_growth_nom  <- 0.030   # 3.0% Nominal House Appreciation
salary_indexation <- 0.035   # 3.5% Salary Indexation
inv_return_nom    <- 0.050   # 5.0% Nominal Investment Return (The variable to test)

# --- The House ---
house_price       <- 400000
notary_fees_pct   <- 0.03    # ~3% in Wallonia/Brussels (registration + fees)
owner_maint_yr    <- 6500    # Yearly ownership costs (Tax/Ins/Maint)

# --- The Mortgage ---
mortgage_rate     <- 0.035   # 3.5% Fixed Rate
mortgage_years    <- 20
max_deductible    <- 2310    # Belgian Tax Shelter limit (Chirec/Longterm)
tax_benefit_rate  <- 0.30    # Approx effective tax relief rate

# --- The "Current" Decision ---
# (This is the specific scenario we simulate first)
current_dp_pct    <- 0.40    # 40% Down Payment
initial_capital   <- 250000  # Total liquid cash available
annual_salary     <- 60000

# ==============================================================================
# PART A: THE SIMULATION ENGINE (FUNCTION)
# ==============================================================================
# We wrap the logic in a function to run it thousands of times for the grid

run_simulation <- function(dp_pct, inv_rate, house_p, cap, sal) {
  
  # Setup
  dp_amount <- house_p * dp_pct
  loan_amount <- house_p - dp_amount
  notary_fees <- house_p * notary_fees_pct
  
  # Check feasibility
  required_cash <- dp_amount + notary_fees
  if (required_cash > cap) return(list(possible = FALSE))
  
  # Mortgage Calc
  monthly_r <- mortgage_rate / 12
  n_pay <- mortgage_years * 12
  
  if (loan_amount > 0) {
    monthly_pay <- loan_amount * (monthly_r * (1 + monthly_r)^n_pay) / ((1 + monthly_r)^n_pay - 1)
  } else {
    monthly_pay <- 0
  }
  annual_pay <- monthly_pay * 12
  
  # Vectors
  sim_years <- mortgage_years
  wealth_vec <- numeric(sim_years + 1)
  
  # Initial State
  current_cap <- cap - required_cash
  current_debt <- loan_amount
  current_house <- house_p
  current_sal <- sal
  savings_rate <- 0.30
  
  # Loop
  for (y in 1:sim_years) {
    # 1. Inflation Adjustments
    current_sal <- current_sal * (1 + salary_indexation)
    annual_savings <- current_sal * savings_rate
    current_maint <- owner_maint_yr * (1 + inflation_rate)^(y-1)
    
    # 2. Mortgage Tax Benefit (Simplified Belgium Model)
    interest_component <- current_debt * mortgage_rate
    deductible <- min(interest_component, max_deductible)
    tax_back <- deductible * tax_benefit_rate
    
    # 3. Cash Flow
    # Out: Mortgage + Maintenance
    # In: Savings + Tax Back
    # Note: If no debt, annual_pay is 0
    net_housing_cost <- annual_pay + current_maint - tax_back
    
    # 4. Capital Investment
    # Capital grows by Inv Return, then we add/subtract the cash flow shortfall/surplus
    current_cap <- current_cap * (1 + inv_rate) + (annual_savings - net_housing_cost)
    
    # 5. Debt Update
    if (current_debt > 0) {
      current_debt <- current_debt * (1 + monthly_r)^12 - (monthly_pay * ((1 + monthly_r)^12 - 1) / monthly_r)
      current_debt <- max(0, current_debt)
    }
    
    # 6. House Appreciation
    current_house <- current_house * (1 + house_growth_nom)
  }
  
  final_wealth <- current_cap + (current_house - current_debt)
  
  return(list(
    possible = TRUE,
    final_wealth = final_wealth,
    final_cap = current_cap,
    final_equity = current_house - current_debt
  ))
}

# ==============================================================================
# PART B: SPECIFIC CASE OUTPUT (CURRENT PARAMETERS)
# ==============================================================================

cat("\n========================================================\n")
cat(" PART 1: ANALYSIS OF SPECIFIC PARAMETERS (Base Case)\n")
cat("========================================================\n")

base_case <- run_simulation(current_dp_pct, inv_return_nom, house_price, initial_capital, annual_salary)

if(base_case$possible) {
  cat(sprintf("Parameters: DP: %.0f%% | Inv.Return: %.1f%% | Mort.Rate: %.1f%%\n", 
              current_dp_pct*100, inv_return_nom*100, mortgage_rate*100))
  cat(sprintf("Final Net Wealth (Year %d): %s EUR\n", 
              mortgage_years, format(round(base_case$final_wealth), big.mark=",")))
  cat(sprintf("Composition: %s (Liquid) + %s (House Equity)\n",
              format(round(base_case$final_cap), big.mark=","),
              format(round(base_case$final_equity), big.mark=",")))
} else {
  cat("ERROR: Current parameters require more cash than available.\n")
}


# ==============================================================================
# PART C: THE OPTIMIZATION MODEL (SENSITIVITY GRID)
# ==============================================================================

cat("\n========================================================\n")
cat(" PART 2: THE OPTIMIZATION EQUATION (Grid Search)\n")
cat("========================================================\n")

# 1. Define the Grid
dp_options <- seq(0.10, 0.90, by = 0.05)       # Down payment 10% to 90%
inv_options <- seq(0.01, 0.08, by = 0.005)     # Investment Return 1% to 8%

grid_results <- expand.grid(DP = dp_options, Return = inv_options)
grid_results$Wealth <- NA
grid_results$Feasible <- NA

# 2. Run Simulation for every combination
for(i in 1:nrow(grid_results)) {
  res <- run_simulation(grid_results$DP[i], grid_results$Return[i], house_price, initial_capital, annual_salary)
  if(res$possible) {
    grid_results$Wealth[i] <- res$final_wealth
    grid_results$Feasible[i] <- "Yes"
  } else {
    grid_results$Feasible[i] <- "No"
  }
}

# Filter out impossible scenarios (where DP > Initial Capital)
grid_clean <- grid_results %>% filter(Feasible == "Yes")

# 3. Calculate the "Theoretical Tipping Point"
# The Equation: Effective Debt Cost ~= Mortgage Rate * (1 - Marginal Tax Impact)
# This is a rough proxy to draw the line on the chart
effective_debt_cost <- mortgage_rate * (1 - (tax_benefit_rate * 0.2)) # Assuming tax benefit covers part of interest

cat(sprintf("Theoretical 'Break-even' Investment Return: ~%.2f%%\n", effective_debt_cost*100))
cat("(If you earn more than this, Math says: MINIMIZE Down Payment)\n")

# ==============================================================================
# PART D: VISUALIZATION
# ==============================================================================

# PLOT 1: The "Equation" Visualized (Curves)
# This shows how Wealth changes as DP changes, for different Return Rates

# Select a few key return rates for clearer lines
key_returns <- c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07)
plot_data <- grid_clean %>% 
  filter(round(Return, 3) %in% key_returns) %>%
  mutate(Return_Label = paste0(Return * 100, "% Return"))

p1 <- ggplot(plot_data, aes(x = DP, y = Wealth, color = as.factor(Return_Label))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(labels = percent, breaks = seq(0.1, 0.9, 0.1)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M €")) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "The 'Leverage Equation': Optimal Down Payment Model",
    subtitle = "Steep Line Up = Pay Cash | Steep Line Down = Maximize Mortgage",
    x = "Down Payment %",
    y = "Final Net Wealth (EUR)",
    color = "Market Performance"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)
cat("Plot 1 Generated: Shows the slope of wealth vs down payment.\n")

# PLOT 2: The Heatmap (Optimization Landscape)
# Shows the exact sweet spot

# Find the maximum wealth for each Return level
optima <- grid_clean %>%
  group_by(Return) %>%
  filter(Wealth == max(Wealth))

p2 <- ggplot(grid_clean, aes(x = DP, y = Return)) +
  geom_tile(aes(fill = Wealth)) +
  scale_fill_gradientn(colors = c("#D73027", "#F46D43", "#FEE08B", "#D9EF8B", "#66BD63", "#1A9850"),
                       labels = label_number(scale_cut = cut_short_scale())) +
  geom_point(data = optima, aes(x = DP, y = Return), color = "black", shape=4, size=3, stroke=2) +
  # Add the Break-even line
  geom_hline(yintercept = mortgage_rate, linetype="dashed", color="white", size=1) +
  annotate("text", x = 0.8, y = mortgage_rate + 0.002, label = "Mortgage Cost Horizon", color="white") +
  scale_x_continuous(labels = percent, expand = c(0,0)) +
  scale_y_continuous(labels = percent, expand = c(0,0)) +
  labs(
    title = "Optimization Grid: Down Payment vs. Investment Return",
    subtitle = "Black 'X' marks the optimal strategy for that return rate",
    x = "Down Payment %",
    y = "Expected Investment Return",
    fill = "Final Wealth"
  ) +
  theme_minimal()

print(p2)
cat("Plot 2 Generated: Heatmap identifying the global maximums.\n")

# ==============================================================================
# FINAL RECOMMENDATION OUTPUT
# ==============================================================================

# Look up the optimal for the USER specified return (inv_return_nom)
best_strategy <- grid_clean %>%
  filter(round(Return, 3) == round(inv_return_nom, 3)) %>%
  filter(Wealth == max(Wealth)) %>%
  slice(1) # Take first if tie

cat("\n========================================================\n")
cat(" FINAL VERDICT (Based on your input parameters)\n")
cat("========================================================\n")
cat(sprintf("Given your expected Investment Return of %.1f%%:\n", inv_return_nom*100))
cat("The Model Suggests:\n")

if(best_strategy$DP <= 0.20) {
  cat(">>> STRATEGY: MAXIMIZE LEVERAGE (Low Down Payment)\n")
  cat("Reason: Your investments outperform the net cost of the mortgage.\n")
} else if (best_strategy$DP >= 0.80) {
  cat(">>> STRATEGY: PAY CASH (High Down Payment)\n")
  cat("Reason: Mortgage interest costs are higher than your investment gains.\n")
} else {
  cat(">>> STRATEGY: BALANCED APPROACH\n")
}

cat(sprintf("Optimal Down Payment: %.0f%%\n", best_strategy$DP * 100))
cat(sprintf("Projected Wealth: %s EUR\n", format(round(best_strategy$Wealth), big.mark=",")))