library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# --- Martingale Simulation ---------------------------------------

simulate_martingale <- function(p_win, n_sim = 1000, max_investment = 1e6) {
  results <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    bet <- 1
    total <- 0
    
    repeat {
      total <- total + bet
      if (total > max_investment) {
        total <- NA
        break
      }
      if (runif(1) < p_win) break
      bet <- bet * 2
    }
    
    results[i] <- total
  }
  
  results
}

# --- UI ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Martingale System Odds Calculator"),
  
  # Friendly top paragraph
  p("The Martingale system is a well-known betting strategy based on doubling your bet after every loss. The idea is simple: when you eventually win, you recover all losses and gain $1. But in practice, this strategy quickly requires enormous amounts of money."),
  p("This simulation lets you choose the probability of winning a bet and explore how much money you'd need to invest to earn just $1 using this system."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Choose Win Probability:"),
      actionButton("coinflip", "Coin Flip (50%)"),
      actionButton("roulette", "Roulette (48.6%)"),
      actionButton("blackjack", "Blackjack (46.2%)"),
      br(), br(),
      
      sliderInput("p_win", "Or set your own probability (%):", min = 10, max = 100, value = 50),
      numericInput("n_sim", "Number of Simulations:", value = 10000, min = 100, max = 100000),
      actionButton("run_sim", "Run Simulation"),
      br(), br(),
      
      uiOutput("summary_box")
    ),
    
    mainPanel(
      h4("Simulation Results"),
      tableOutput("investment_table")
    )
  )
)

# --- Server -----------------------------------------------------------

server <- function(input, output, session) {
  
  # Probability buttons
  observeEvent(input$coinflip, {
    updateSliderInput(session, "p_win", value = 50)
  })
  observeEvent(input$roulette, {
    updateSliderInput(session, "p_win", value = round(18 / 37 * 100, 1))
  })
  observeEvent(input$blackjack, {
    updateSliderInput(session, "p_win", value = 46.2)
  })
  
  # Simulation
  results <- eventReactive(input$run_sim, {
    p <- input$p_win / 100
    simulate_martingale(p_win = p, n_sim = input$n_sim)
  })
  
  # Table
  output$investment_table <- renderTable({
    req(results())
    
    invested <- results()
    threshold_vals <- 2^(1:20) - 1  # keep this numeric for computation
    
    cum_prob <- sapply(threshold_vals, function(x) {
      mean(ifelse(is.na(invested), FALSE, invested <= x)) * 100
    })
    
    thresholds <- scales::comma(threshold_vals) #ajouter des virgules pour aider la lecture
    
    success_tbl <- tibble(
      `Investment ($)` = thresholds,
      `Cumulative Success Probability (%)` = formatC(cum_prob, format = "f", digits = 2)
    )
    
    fail_row <- tibble(
      `Investment ($)` = "Failed (No Win)",
      `Cumulative Success Probability (%)` = as.character(sum(is.na(invested)))
    )
    
    bind_rows(success_tbl, fail_row)
  })
  
  # Summary box
  output$summary_box <- renderUI({
    req(results())
    invested <- results()
    successful <- sum(!is.na(invested))
    failed <- sum(is.na(invested))
    
    HTML(paste0(
      "✅ Successes: ", successful, "<br>",
      "❌ Failures (bankrupt): ", failed
    ))
  })
  
}

# --- Run App ----------------------------------------------------------

shinyApp(ui, server)
