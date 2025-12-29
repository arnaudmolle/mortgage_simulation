library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(bslib)
library(bsicons)
library(thematic)

thematic_shiny()

source("model/defaults.R")
source("model/validation.R")
source("model/generators.R")
source("model/renovation.R")
source("model/scenarios.R")
source("model/runner.R")
source("model/aggregation.R")
source("model/optimize_dp.R")


ui <- fluidPage(
  titlePanel("Mortgage vs Rent Monte Carlo Model"),

  sidebarLayout(
    sidebarPanel(
      h4("Personal Situation"),
      
      numericInput("initial_capital", "Available capital (€)", 250000, step = 10000),
      numericInput("annual_salary", "Annual salary (€)", 50000, step = 5000),
      sliderInput("savings_rate", "Savings rate (%)", 0, 100, 30, step = 5),
      
      h4("Property"),
      numericInput("house_price", "House price (€)", 400000, step = 10000),
      sliderInput("down_payment", "Down payment (%)", 10, 95, 20),
      sliderInput("mortgage_rate", "Mortgage rate (%)", 1, 6, 3.5, step = 0.1),
      
      h4("Simulation Settings"),
      numericInput("n_sim", "Monte Carlo simulations", 5000, step = 1000),
      
      # Advanced options - now always visible but collapsible
      h4("Monte Carlo Parameters"),
      
      h5("Investment Returns"),
      fluidRow(
        column(6, sliderInput("inv_mean", "Mean (%)", -5, 15, 4, step = 0.5)),
        column(6, sliderInput("inv_sd", "Std Dev (%)", 5, 30, 15, step = 1))
      ),
      
      h5("House Appreciation"),
      fluidRow(
        column(6, sliderInput("house_mean", "Mean (%)", -2, 10, 3, step = 0.5)),
        column(6, sliderInput("house_sd", "Std Dev (%)", 1, 20, 8, step = 1))
      ),
      
      h5("Inflation"),
      fluidRow(
        column(6, sliderInput("inflation_mean", "Mean (%)", 0, 8, 2.4, step = 0.2)),
        column(6, sliderInput("inflation_sd", "Std Dev (%)", 0.5, 5, 1.5, step = 0.2))
      ),
      
      h5("Salary Growth"),
      fluidRow(
        column(6, sliderInput("salary_mean", "Mean (%)", -2, 10, 3.5, step = 0.5)),
        column(6, sliderInput("salary_sd", "Std Dev (%)", 0, 10, 2.5, step = 0.5))
      ),
      
      h5("Correlations"),
      sliderInput("cor_inv_house", "Investments vs Housing", -0.5, 0.5, -0.1, step = 0.05),

      actionButton("run", "Run simulation", class = "btn-primary btn-lg", width = "100%")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Eligibility & Summary",
                 htmlOutput("eligibility_text"),
                 br(),
                 tableOutput("summary_table")),
        tabPanel("Wealth Trajectories", plotOutput("wealth_plot")),
        tabPanel("Risk Analysis", 
                 fluidRow(
                   column(6, tableOutput("risk_table")),
                   column(6, plotOutput("risk_dist_plot"))
                 ),
                 plotOutput("risk_box_plot")),
        tabPanel("Optimal Down Payment",
                 htmlOutput("optimal_dp_text"),
                 plotOutput("optimal_dp_plot"),
                 tableOutput("optimal_comparison_table"))
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

  # Eligibility output
  output$eligibility_text <- renderUI({
    elig <- eligibility()
    p_params <- params()
    
    html <- paste0(
      "<h4>Scenario Eligibility Check</h4>",
      "<p><strong>Available Capital:</strong> €", format(round(p_params$initial_capital), big.mark = ","), "</p>",
      "<p><strong>Annual Salary:</strong> €", format(round(p_params$annual_salary), big.mark = ","), "</p>",
      "<p><strong>Notary Fees (3%):</strong> €", format(round(elig$notary_fees), big.mark = ","), "</p>",
      "<hr>",
      "<p><strong>Cash Purchase:</strong> ",
      if(elig$can_buy_cash) {
        paste0("✓ ELIGIBLE (need €", format(round(elig$total_for_cash), big.mark = ","), ")")
      } else {
        paste0("✗ NOT ELIGIBLE (need €", format(round(elig$total_for_cash), big.mark = ","), ")")
      },
      "</p>",
      "<p><strong>Mortgage Purchase:</strong> ",
      if(elig$can_buy_mortgage) {
        paste0("✓ ELIGIBLE (need €", format(round(elig$total_for_mortgage_min), big.mark = ","), " minimum)")
      } else {
        paste0("✗ NOT ELIGIBLE (need €", format(round(elig$total_for_mortgage_min), big.mark = ","), ")")
      },
      "</p>",
      "<p><strong>Rent:</strong> ✓ ALWAYS ELIGIBLE</p>"
    )
    
    HTML(html)
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
  }, striped = TRUE, hover = TRUE)

  output$wealth_plot <- renderPlot({
    req(results())
    plot_wealth(results())
  })

  output$risk_table <- renderTable({
    req(results())
    risk_table(results())
  }, striped = TRUE, hover = TRUE)

  output$risk_dist_plot <- renderPlot({
    req(results())
    plot_risk_distribution(results())
  })

  output$risk_box_plot <- renderPlot({
    req(results())
    plot_risk_boxplot(results())
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
      return(HTML("<p style='color: red;'><strong>Mortgage not eligible. Optimal down payment analysis not available.</strong></p>"))
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
    
    html <- paste0(
      "<h4>Optimal Down Payment Analysis</h4>",
      "<p><strong>Optimal Down Payment:</strong> ", sprintf("%.0f%%", opt_dp * 100), "</p>",
      "<p><strong>Down Payment Amount:</strong> €", format(round(opt_dp * p$house_price), big.mark = ","), "</p>",
      "<p><strong>Loan Amount:</strong> €", format(round((1 - opt_dp) * p$house_price), big.mark = ","), "</p>",
      "<p><strong>Remaining Capital:</strong> €", format(round(p$initial_capital - opt_dp * p$house_price - p$notary_fees_pct * p$house_price), big.mark = ","), "</p>",
      "<hr>",
      "<p><strong>Expected Final Wealth (Year 30):</strong></p>",
      "<ul>",
      "<li>Median: €", format(round(opt_wealth), big.mark = ","), "</li>",
      "<li>10th percentile: €", format(round(res$p10[opt_idx]), big.mark = ","), "</li>",
      "<li>90th percentile: €", format(round(res$p90[opt_idx]), big.mark = ","), "</li>",
      "</ul>",
      "<p><strong>Improvement vs 20% down:</strong> €", format(round(improvement), big.mark = ","), 
      " (+", sprintf("%.1f%%", improvement_pct), ")</p>"
    )
    
    HTML(html)
  })

  output$optimal_dp_plot <- renderPlot({
    res <- optimal_results()
    
    if (is.null(res)) {
      return(NULL)
    }
    
    opt_idx <- which.max(res$median_wealth)
    
    ggplot(res, aes(x = dp * 100, y = median_wealth)) +
      geom_line(color = "#A23B72", linewidth = 1.5) +
      geom_ribbon(aes(ymin = p10, ymax = p90), alpha = 0.2, fill = "#A23B72") +
      geom_point(data = res[opt_idx, ], 
                 aes(x = dp * 100, y = median_wealth),
                 color = "#06A77D", size = 5, shape = 21, fill = "#06A77D", stroke = 2) +
      geom_vline(xintercept = res$dp[opt_idx] * 100, linetype = "dashed", color = "#06A77D", linewidth = 1) +
      scale_y_continuous(labels = scales::comma_format(suffix = " €")) +
      scale_x_continuous(breaks = seq(10, 100, 10)) +
      labs(
        title = "Optimal Down Payment Analysis",
        subtitle = "Shaded area = 10th-90th percentile range | Green dot = optimal down payment",
        x = "Down Payment (%)",
        y = "Final Wealth - Median (EUR)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 11)
      )
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
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui, server)
