# Mortgage vs Rent Monte Carlo Simulation

A comprehensive Shiny application for analyzing the financial outcomes of purchasing a home via mortgage, renting, or buying with cash. The app uses Monte Carlo simulation to model 30-year (or custom duration) wealth trajectories under various economic scenarios.

## Features

### Core Analysis
- **Three Scenarios**: Compare mortgage purchase, renting, and cash purchase strategies
- **Monte Carlo Simulation**: 5,000+ correlated simulations (customizable) to capture market uncertainty
- **Dynamic Horizons**: Adjust mortgage duration from 5 to 40 years
- **Risk Analysis**: Comprehensive risk metrics including median outcomes, percentile ranges, and coefficient of variation

### User Inputs

#### Personal Situation
- **Available Capital**: Starting investable assets (€)
- **Annual Salary**: Base income for savings calculations (€)
- **Savings Rate**: Percentage of income saved annually (0-100%)

#### Property Details
- **House Price**: Property cost (€)
- **Down Payment**: Percentage of house price for mortgage purchase (10-95%)
- **Mortgage Rate**: Annual interest rate (1-6%)
- **Mortgage Duration**: Loan term in years (5-40)

#### Monte Carlo Parameters
Fine-tune all economic variables and their correlations:
- **Investment Returns**: Mean and standard deviation of portfolio growth
- **House Appreciation**: Mean and standard deviation of property value growth
- **Inflation**: Mean and standard deviation of general price level changes
- **Salary Growth**: Mean and standard deviation of income growth
- **Correlations**: Relationships between investment returns, inflation, house appreciation, and salary growth

### Simulation Settings
- **Number of Simulations**: Monte Carlo iterations (100-100,000)
- All parameters are interconnected through a 4×4 correlation matrix

## Outputs & Visualizations

### Eligibility & Summary Tab
- **Value Boxes**: Quick overview of available capital and purchase eligibility
- **Wealth Projection Table**: Median final wealth (Year 30) with 10th and 90th percentile ranges

### Wealth Trajectories Tab
- **Median Wealth Paths**: Time series showing median wealth over the mortgage/analysis period
- **Percentile Bands**: 25th-75th percentile ranges showing distribution spread
- Automatically hidden when not eligible for mortgage purchase

### Risk Analysis Tab
- **Risk Metrics Table**: Comparative statistics including median, percentiles, standard deviation, and coefficient of variation
- **Density Plot**: Probability distribution of final wealth across scenarios
- **Box Plot**: Visual comparison of wealth distributions (IQR, median, outliers)

### Optimal Down Payment Tab
- **Efficient Frontier**: Median wealth outcome across down payment percentages (10-95%)
- **Confidence Range**: 10th-90th percentile envelope
- **Optimal Point**: Highlighted down payment percentage maximizing median terminal wealth
- **Comparison Table**: Side-by-side metrics for key down payment levels

### Model Parameters Tab
- **Correlation Matrix**: Interactive 4×4 matrix showing relationships between all Monte Carlo variables
- Updates dynamically as you adjust correlation sliders

## How It Works

### Simulation Process

1. **Path Generation**
   - Generate correlated random normal variables using Cholesky decomposition
   - Apply user-specified means and standard deviations to create economic paths for:
     - Investment portfolio returns
     - Inflation rates
     - House appreciation
     - Salary growth

2. **Scenario Simulation** (for each of 5,000+ simulations)
   - **Mortgage Scenario**: Monthly loan payments, annual owner costs, tax benefits from mortgage interest deduction, wealth accumulated through home equity
   - **Rent Scenario**: Invest full initial capital, save annual surplus, pay rent indexed to inflation, no property ownership
   - **Cash Purchase Scenario**: Full down payment with no mortgage, property ownership costs, reinvestment of savings

3. **Results Aggregation**
   - Extract final wealth and intermediate liquid capital
   - Calculate risk metrics across all simulations
   - Determine scenario eligibility based on available capital

### Key Assumptions

- **Mortgage Term**: Adjustable from 5 to 40 years
- **Property Ownership Costs**: €6,500/year (property tax, insurance, maintenance, utilities)
- **Rental Yield**: 4.24% with 1.15x markup applied
- **Tax Benefits**: 45% deduction on mortgage interest (capped at €2,310/year)
- **Minimum Down Payment**: 10% of house price
- **Notary Fees**: 3% of house price (one-time, required for mortgage or cash purchase)
- **Renovation Shocks**: Optional random expenses (15% probability, 15% of house price) occurring between years 10-25 (clipped to actual simulation horizon)

## Installation & Usage

### Requirements
- R 4.0+
- Shiny
- ggplot2, dplyr, tidyr, scales
- bslib, bsicons, thematic

### Running the App

```r
# From R console in the app directory
shiny::runApp()

# Or from command line
Rscript -e "shiny::runApp('.')"
```

The app opens at `http://localhost:3838`

## File Structure

```
mortgage_simulation/
├── app.R                          # Main Shiny application
├── model/
│   ├── defaults.R                 # Default parameter values
│   ├── generators.R               # Monte Carlo path generation
│   ├── scenarios.R                # Mortgage, rent, and cash scenarios
│   ├── runner.R                   # Orchestration function
│   ├── aggregation.R              # Results processing and plotting
│   ├── optimize_dp.R              # Optimal down payment analysis
│   ├── renovation.R               # Renovation shock generation
│   └── validation.R               # Parameter validation
├── rsconnect/                     # Shiny server deployment config
└── README.md                      # This file
```

## Key Insights

### When to Use Each Scenario

- **Mortgage Purchase**: Optimal when property appreciation is expected and tax benefits matter
- **Renting**: Lower risk, flexibility, suits those prioritizing liquidity
- **Cash Purchase**: Eliminates interest costs but reduces investment flexibility

### Interpreting Results

- **Median Wealth**: Expected outcome at 50th percentile
- **P10/P90 Range**: 80% probability outcome falls within this band
- **Coefficient of Variation**: Risk per unit return; lower is better
- **Efficient Frontier**: Down payment percentage maximizing expected terminal wealth

## Customization

All economic parameters and correlations can be adjusted through the UI. Common modifications:

- **Bull Market**: Increase investment mean/SD, house appreciation
- **Recession Scenario**: Decrease investment mean, increase volatility
- **High Inflation**: Increase inflation mean, adjust correlations
- **Rising Rates**: Increase mortgage rate, adjust correlation between rates and other variables

## Limitations

- Assumes constant savings rate and no major life changes
- Mortgage payments are fixed (no variable rate mortgages)
- No consideration of transaction costs beyond notary fees
- Renovation shocks are simplified (fixed percentage, random timing)
- No tax optimization beyond mortgage interest deduction
- Assumes investment portfolio with constant allocation

## Contact & Support

For issues, questions, or suggestions regarding this simulation, please refer to the project repository or contact the development team.

---

**Last Updated**: December 2025  
**Version**: 1.0  
**Status**: Active Development
