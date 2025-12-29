library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(bslib)      # For modern UI components
library(bsicons)    # For icons
library(thematic)   # To auto-style plots

# Automatically theme plots to match the UI
thematic_shiny()

# Load models (Assumed to be in your working directory)
source("model/defaults.R")
source("model/validation.R")
source("model/generators.R")
source("model/renovation.R")
source("model/scenarios.R")
source("model/runner.R")
source("model/aggregation.R")
source("model/optimize_dp.R")

ui <- page_sidebar(
  title = div(bs_icon("graph-up-arrow"), " Mortgage vs Rent Monte Carlo"),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr", # Clean, professional financial theme
    primary = "#2C3E50",
    "card-cap-bg" = "#F8F9FA"
  ),

  sidebar = sidebar(
    width = 400, # Slightly wider to accommodate side-by-side inputs
    bg = "#f8f9fa",
    
    # Run button at top for easy access
    actionButton("run", "Run Simulation", icon = icon("play"), class = "btn-primary btn-lg w-100 mb-3"),
    
    accordion(
      open = c("Personal", "Property"), # Open primary sections by default
      
      accordion_panel(
        "Personal Situation",
        icon = bs_icon("person-vcard"),
        value = "Personal",
        numericInput("initial_capital", "Available capital (€)", 250000, step = 10000),
        numericInput("annual_salary", "Annual salary (€)", 50000, step = 5000),
        sliderInput("savings_rate", "Savings rate (%)", 0, 100, 30, step = 5)
      ),
      
      accordion_panel(
        "Property Details",
        icon = bs_icon("house"),
        value = "Property",
        numericInput("house_price", "House price (€)", 400000, step = 10000),
        sliderInput("down_payment", "Down payment (%)", 10, 95, 20),
        sliderInput("mortgage_rate", "Mortgage rate (%)", 1, 6, 3.5, step = 0.1),
        sliderInput("mortgage_years", "Mortgage duration (years)", 5, 40, 30, step = 1)
      ),
      
      accordion_panel(
        "Market Parameters",
        icon = bs_icon("sliders"),
        value = "Advanced",
        numericInput("n_sim", "Monte Carlo simulations", 5000, step = 1000),
        
        # Grouping advanced inputs neatly
        h6("Investment Returns", class = "mt-3 text-primary"),
        layout_columns(
          col_widths = c(6, 6),
          sliderInput("inv_mean", "Mean (%)", -5, 15, 4, step = 0.5),
          sliderInput("inv_sd", "SD (%)", 5, 30, 15, step = 1)
        ),
        
        h6("House Appreciation", class = "mt-2 text-primary"),
        layout_columns(
          col_widths = c(6, 6),
          sliderInput("house_mean", "Mean (%)", -2, 10, 3, step = 0.5),
          sliderInput("house_sd", "SD (%)", 1, 20, 8, step = 1)
        ),
        
        h6("Inflation", class = "mt-2 text-primary"),
        layout_columns(
          col_widths = c(6, 6),
          sliderInput("inflation_mean", "Mean (%)", 0, 8, 2.4, step = 0.2),
          sliderInput("inflation_sd", "SD (%)", 0.5, 5, 1.5, step = 0.2)
        ),
        
        h6("Salary Growth", class = "mt-2 text-primary"),
        layout_columns(
          col_widths = c(6, 6),
          sliderInput("salary_mean", "Mean (%)", -2, 10, 3.5, step = 0.5),
          sliderInput("salary_sd", "SD (%)", 0, 10, 2.5, step = 0.5)
        ),
        
        h6("Correlations", class = "mt-2 text-primary"),
        sliderInput("cor_inv_house", "Investments vs Housing", -0.5, 0.5, -0.1, step = 0.05)
      )
    )
  ),

  # Main Content Area
  navset_card_underline(
    title = "Analysis Results",
    
    # --- Tab 1: Summary ---
    nav_panel(
      "Eligibility & Summary",
      icon = bs_icon("clipboard-check"),
      layout_columns(
        col_widths = 12,
        uiOutput("eligibility_cards") # Using Value Boxes now
      ),
      card(
        card_header("Wealth Projection (Year 30)"),
        tableOutput("summary_table")
      )
    ),
    
    # --- Tab 2: Trajectories ---
    nav_panel(
      "Wealth Trajectories",
      icon = bs_icon("graph-up"),
      card(
        full_screen = TRUE,
        card_header("Monte Carlo Paths"),
        plotOutput("wealth_plot", height = "600px")
      )
    ),
    
    # --- Tab 3: Risk ---
    nav_panel(
      "Risk Analysis",
      icon = bs_icon("shield-exclamation"),
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Risk Metrics"),
          tableOutput("risk_table")
        ),
        card(
          card_header("Terminal Wealth Distribution"),
          plotOutput("risk_dist_plot")
        )
      ),
      card(
        card_header("Wealth Spread Comparison"),
        plotOutput("risk_box_plot", height = "300px")
      )
    ),
    
    # --- Tab 4: Optimization ---
    nav_panel(
      "Optimal Down Payment",
      icon = bs_icon("bullseye"),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Optimization Result"),
          htmlOutput("optimal_dp_text")
        ),
        card(
          full_screen = TRUE,
          card_header("Efficient Frontier"),
          plotOutput("optimal_dp_plot"),
          hr(),
          h5("Detailed Comparison", class = "card-title fs-6"),
          tableOutput("optimal_comparison_table")
        )
      )
    ),
    
    # --- Tab 5: Model Parameters ---
    nav_panel(
      "Model Parameters",
      icon = bs_icon("sliders"),
      card(
        card_header("Correlation Matrix"),
        p("4x4 correlation matrix for Monte Carlo variables:"),
        tableOutput("correlation_matrix_table"),
        p(class = "text-muted small mt-3",
          "Investment Returns vs Inflation, House Appreciation, and Salary Growth;",
          br(),
          "House Appreciation vs Salary Growth")
      )
    )
  )
)

server <- function(input, output, session) {

  params <- reactive({
    p <- default_params()

    # Personal situation
    p$initial_capital <- input$initial_capital
    p$annual_salary <- input$annual_salary
    p$savings_rate <- input$savings_rate / 100

    # Property
    p$house_price <- input$house_price
    p$down_payment_pct <- input$down_payment / 100
    p$mortgage_rate <- input$mortgage_rate / 100

    # Simulation
    p$n_sim <- input$n_sim

    # Advanced MC parameters
    p$inv_mean <- input$inv_mean / 100
    p$inv_sd <- input$inv_sd / 100
    p$house_mean <- input$house_mean / 100
    p$house_sd <- input$house_sd / 100
    p$inflation_mean <- input$inflation_mean / 100
    p$inflation_sd <- input$inflation_sd / 100
    p$salary_mean <- input$salary_mean / 100
    p$salary_sd <- input$salary_sd / 100
    p$cor_inv_house <- input$cor_inv_house
    p$mortgage_years <- input$mortgage_years

    p
  })

  # Check scenario eligibility
  eligibility <- reactive({
    p <- params()
    
    notary_fees <- p$house_price * p$notary_fees_pct
    min_down_payment <- p$house_price * 0.10
    total_for_cash <- p$house_price + notary_fees
    total_for_mortgage_min <- min_down_payment + notary_fees
    
    list(
      can_buy_cash = p$initial_capital >= total_for_cash,
      can_buy_mortgage = p$initial_capital >= total_for_mortgage_min,
      can_rent = TRUE,
      notary_fees = notary_fees,
      total_for_cash = total_for_cash,
      total_for_mortgage_min = total_for_mortgage_min
    )
  })

  results <- eventReactive(input$run, {
    withProgress(message = "Running Monte Carlo simulation...", {
      run_model(params())
    })
  })

  # --- NEW: Improved Eligibility Display with Value Boxes ---
  output$eligibility_cards <- renderUI({
    elig <- eligibility()
    p_params <- params()
    
    # Helper to format money
    fmt <- function(x) paste0("€", format(round(x), big.mark = ","))
    
    layout_columns(
      value_box(
        title = "Available Capital",
        value = fmt(p_params$initial_capital),
        showcase = bs_icon("wallet2"),
        theme = "primary"
      ),
      value_box(
        title = "Mortgage Purchase",
        value = if(elig$can_buy_mortgage) "Eligible" else "Not Eligible",
        p(if(elig$can_buy_mortgage) paste("Min Req:", fmt(elig$total_for_mortgage_min)) else paste("Need", fmt(elig$total_for_mortgage_min))),
        showcase = if(elig$can_buy_mortgage) bs_icon("check-circle") else bs_icon("x-circle"),
        theme = if(elig$can_buy_mortgage) "teal" else "danger"
      ),
      value_box(
        title = "Cash Purchase",
        value = if(elig$can_buy_cash) "Eligible" else "Not Eligible",
        p(paste("Total Req:", fmt(elig$total_for_cash))),
        showcase = if(elig$can_buy_cash) bs_icon("cash-coin") else bs_icon("bank"),
        theme = if(elig$can_buy_cash) "teal" else "secondary"
      )
    )
  })

  # Summary table
  output$summary_table <- renderTable({
    req(results())
    
    res <- results()
    p_params <- params()
    elig <- eligibility()
    
    # Extract final wealth for each scenario
    mort_wealth <- sapply(res, function(x) tail(x$mortgage$wealth, 1))
    rent_wealth <- sapply(res, function(x) tail(x$rent$wealth, 1))
    cash_wealth <- sapply(res, function(x) tail(x$cash$wealth, 1))
    
    # Build results table with only eligible scenarios
    results_list <- list()
    
    if (elig$can_buy_mortgage) {
      results_list$Mortgage <- list(
        median = median(mort_wealth),
        p10 = quantile(mort_wealth, 0.10),
        p90 = quantile(mort_wealth, 0.90)
      )
    }
    
    # Always show rent
    results_list$Rent <- list(
      median = median(rent_wealth),
      p10 = quantile(rent_wealth, 0.10),
      p90 = quantile(rent_wealth, 0.90)
    )
    
    if (elig$can_buy_cash) {
      results_list$`Cash Purchase` <- list(
        median = median(cash_wealth),
        p10 = quantile(cash_wealth, 0.10),
        p90 = quantile(cash_wealth, 0.90)
      )
    }
    
    # Convert to data frame
    scenario_names <- names(results_list)
    data.frame(
      Scenario = scenario_names,
      Median = sapply(results_list, function(x) format(round(x$median), big.mark = ",")),
      P10 = sapply(results_list, function(x) format(round(x$p10), big.mark = ",")),
      P90 = sapply(results_list, function(x) format(round(x$p90), big.mark = ","))
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$wealth_plot <- renderPlot({
    req(results())
    elig <- eligibility()
    if (!elig$can_buy_mortgage) {
      return(NULL)
    }
    plot_wealth(results(), elig)
  })

  output$risk_table <- renderTable({
    req(results())
    risk_table(results())
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$risk_dist_plot <- renderPlot({
    req(results())
    plot_risk_distribution(results(), eligibility())
  })

  output$risk_box_plot <- renderPlot({
    req(results())
    plot_risk_boxplot(results(), eligibility())
  })

  # Optimal down payment analysis
  optimal_results <- eventReactive(input$run, {
    req(results())
    
    withProgress(message = "Calculating optimal down payment...", {
      p <- params()
      elig <- eligibility()
      
      if (!elig$can_buy_mortgage) {
        return(NULL)
      }
      
      # Generate down payment grid
      max_dp <- (p$initial_capital - elig$notary_fees) / p$house_price
      dp_grid <- seq(0.10, min(0.95, max_dp), by = 0.05)
      
      optimize_down_payment(p, dp_grid)
    })
  })

  output$optimal_dp_text <- renderUI({
    res <- optimal_results()
    
    if (is.null(res)) {
      return(div(class = "alert alert-danger", "Mortgage not eligible. Optimal down payment analysis not available."))
    }
    
    opt_idx <- which.max(res$median_wealth)
    opt_dp <- res$dp[opt_idx]
    opt_wealth <- res$median_wealth[opt_idx]
    p <- params()
    
    # Compare to default 20%
    default_idx <- which.min(abs(res$dp - 0.20))
    default_wealth <- res$median_wealth[default_idx]
    improvement <- opt_wealth - default_wealth
    improvement_pct <- (improvement / default_wealth) * 100
    
    div(
      h4(class="text-success", paste0("Optimal Down Payment: ", sprintf("%.0f%%", opt_dp * 100))),
      p(class="lead", paste0("Median Wealth: €", format(round(opt_wealth), big.mark = ","))),
      hr(),
      tags$ul(class="list-unstyled",
        tags$li(strong("Down Payment Amount: "), "€", format(round(opt_dp * p$house_price), big.mark = ",")),
        tags$li(strong("Loan Amount: "), "€", format(round((1 - opt_dp) * p$house_price), big.mark = ",")),
        tags$li(strong("Remaining Capital: "), "€", format(round(p$initial_capital - opt_dp * p$house_price - p$notary_fees_pct * p$house_price), big.mark = ",")),
        tags$li(class="mt-2 text-primary", strong("Gain vs 20% Down: "), "€", format(round(improvement), big.mark = ","), 
                paste0(" (+", sprintf("%.1f%%", improvement_pct), ")"))
      )
    )
  })

  output$optimal_dp_plot <- renderPlot({
    res <- optimal_results()
    
    if (is.null(res)) {
      return(NULL)
    }
    
    opt_idx <- which.max(res$median_wealth)
    
    ggplot(res, aes(x = dp * 100, y = median_wealth)) +
      geom_line(color = "#2C3E50", linewidth = 1.5) +
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2, fill = "#2C3E50") +
      geom_point(data = res[opt_idx, ], 
                 aes(x = dp * 100, y = median_wealth),
                 color = "#18BC9C", size = 5, shape = 21, fill = "#18BC9C", stroke = 2) +
      geom_vline(xintercept = res$dp[opt_idx] * 100, linetype = "dashed", color = "#18BC9C", linewidth = 1) +
      scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
      scale_x_continuous(breaks = seq(10, 100, 10)) +
      labs(
        title = "Optimal Down Payment Analysis",
        subtitle = "Shaded area = 10th-90th percentile range | Green dot = optimal down payment",
        x = "Down Payment (%)",
        y = "Final Wealth - Median (EUR)"
      ) +
      theme_minimal(base_size = 14)
  })

  output$optimal_comparison_table <- renderTable({
    res <- optimal_results()
    
    if (is.null(res)) {
      return(NULL)
    }
    
    # Show key down payment levels
    indices <- which(res$dp %in% c(0.10, 0.20, 0.30, 0.50) | res$dp == res$dp[which.max(res$median_wealth)])
    
    comparison <- res[sort(unique(indices)), ] %>%
      mutate(
        DP = paste0(sprintf("%.0f%%", dp * 100)),
        Median = format(round(median_wealth), big.mark = ","),
        P10 = format(round(p10), big.mark = ","),
        P90 = format(round(p90), big.mark = ","),
        CV = sprintf("%.3f", cv)
      ) %>%
      select(DP, Median, P10, P90, CV)
    
    colnames(comparison) <- c("Down Payment", "Median €", "10th % €", "90th % €", "Risk (CV)")
    
    comparison
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$correlation_matrix_table <- renderTable({
    p <- params()
    
    # Build the correlation matrix
    cor_matrix <- matrix(c(
      1,  p$cor_inv_infl, p$cor_inv_house, p$cor_inv_salary,
      p$cor_inv_infl, 1, -0.2, 0.6,
      p$cor_inv_house, -0.2, 1, p$cor_house_salary,
      p$cor_inv_salary, 0.6, p$cor_house_salary, 1
    ), 4, byrow = TRUE)
    
    # Add row and column names
    var_names <- c("Investment", "Inflation", "House Appr.", "Salary Gr.")
    rownames(cor_matrix) <- var_names
    colnames(cor_matrix) <- var_names
    
    # Convert to data frame with variable names as first column
    df <- data.frame(Variable = rownames(cor_matrix), cor_matrix, check.names = FALSE)
    
    # Round to 3 decimal places
    df[, -1] <- round(df[, -1], 3)
    
    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")
}

shinyApp(ui, server)