library(shiny)
library(tidyquant)
library(tidyverse)
library(lubridate)
library(zoo)
library(DT)

# --- Strategy Logic Functions ---
# Passive DCA
passive_dca <- function(prices, amount, freq = "monthly") {
  invest_dates <- seq.Date(from = min(prices$date), to = max(prices$date), by = freq)
  df <- prices %>%
    filter(date %in% invest_dates) %>%
    mutate(invest = amount,
           shares = invest / adjusted,
           cum_invest = cumsum(invest),
           cum_shares = cumsum(shares),
           market_value = cum_shares * adjusted)
  return(df)
}

# Active DCA (triggered by consecutive price drops)
active_dca <- function(prices, amount, drop_percent = 2, days = 1) {
  df <- prices %>%
    mutate(pct_change = (adjusted - lag(adjusted)) / lag(adjusted) * 100) %>%
    mutate(trigger = rollapplyr(pct_change, width = days, FUN = function(x) all(x < -drop_percent), fill = FALSE)) %>%
    filter(trigger) %>%
    mutate(invest = amount,
           shares = invest / adjusted,
           cum_invest = cumsum(invest),
           cum_shares = cumsum(shares),
           market_value = cum_shares * adjusted)
  return(df)
}

# Value Averaging Strategy
value_averaging <- function(prices, target_increment = 2000) {
  df <- prices %>%
    filter(date %in% seq.Date(min(date), max(date), by = "month")) %>%
    mutate(target_value = seq(from = target_increment, by = target_increment, length.out = n()),
           buy = NA_real_,
           cum_shares = 0,
           cum_value = 0)
  
  for (i in 1:nrow(df)) {
    current_price <- df$adjusted[i]
    
    if (i == 1) {
      shares <- df$target_value[i] / current_price
    } else {
      prev_shares <- df$cum_shares[i - 1]
      required_value <- df$target_value[i]
      current_value <- prev_shares * current_price
      to_invest <- required_value - current_value
      
      # 👇 Do not allow selling: if to_invest < 0, then shares = 0
      shares <- ifelse(to_invest > 0, to_invest / current_price, 0)
    }
    
    df$buy[i] <- shares
    df$cum_shares[i] <- ifelse(i == 1, shares, df$cum_shares[i - 1] + shares)
    df$cum_value[i] <- df$cum_shares[i] * current_price
    df$invest[i] <- shares * current_price
    df$cum_invest[i] <- sum(df$invest[1:i])
    df$market_value[i] <- df$cum_value[i]
  }
  
  return(df)
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Investment Strategy Simulator"),
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Enter Stock Ticker (e.g., AAPL)", value = "AAPL"),
      dateInput("start", "Start Date", value = "2020-01-01"),
      dateInput("end", "End Date", value = Sys.Date()),
      selectInput("strategy", "Select Strategy",
                  choices = c("Passive DCA", "Active DCA", "Value Averaging")),
      conditionalPanel(
        condition = "input.strategy == 'Passive DCA'",
        numericInput("amount_passive", "Investment Amount per Period", 1000),
        selectInput("freq", "Investment Frequency", choices = c("months", "weeks"))
      ),
      conditionalPanel(
        condition = "input.strategy == 'Active DCA'",
        numericInput("amount_active", "Investment Amount per Trigger", 1000),
        numericInput("drop_pct", "Price Drop Threshold (%)", value = 2),
        numericInput("drop_days", "Consecutive Drop Days", value = 1)
      ),
      conditionalPanel(
        condition = "input.strategy == 'Value Averaging'",
        numericInput("value_increment", "Target Value Increase per Period", 2000)
      ),
      actionButton("go", "Run Simulation")
    ),
    mainPanel(
      plotOutput("plot"),
      DTOutput("table"),
      verbatimTextOutput("summary")
    )
  )
)

# --- Server ---
server <- function(input, output) {
  observeEvent(input$go, {
    req(input$ticker, input$start, input$end)
    
    prices <- tq_get(input$ticker,
                     from = input$start,
                     to = input$end) %>%
      select(date, adjusted)
    
    result <- reactive({
      switch(input$strategy,
             "Passive DCA" = passive_dca(prices, input$amount_passive, input$freq),
             "Active DCA" = active_dca(prices, input$amount_active, input$drop_pct, input$drop_days),
             "Value Averaging" = value_averaging(prices, input$value_increment))
    })
    
    output$plot <- renderPlot({
      df <- result()
      ggplot(df, aes(x = date)) +
        geom_line(aes(y = market_value), color = "blue", size = 1.2, alpha = 0.9) +
        geom_line(aes(y = cum_invest), color = "red", size = 1.2, alpha = 0.7) +
        geom_point(aes(y = cum_invest), color = "green") + 
        labs(title = paste("Investment Strategy Simulation -", input$strategy),
             y = "Amount (Market Value / Invested)", x = "Date") +
        theme_minimal()
    })
    
    output$table <- DT::renderDT(result())
    
    output$summary <- renderText({
      df <- result()
      if (nrow(df) == 0) return("No transaction data.")
      final_value <- tail(df$market_value, 1)
      final_invest <- tail(df$cum_invest, 1)
      profit <- final_value - final_invest
      rate <- (final_value / final_invest - 1) * 100
      sprintf("Total Invested: %.2f\nCurrent Market Value: %.2f\nProfit: %.2f\nReturn Rate: %.2f%%",
              final_invest, final_value, profit, rate)
    })
  })
}

# --- Launch App ---
shinyApp(ui = ui, server = server)
