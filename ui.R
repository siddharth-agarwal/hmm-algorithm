# ui.R

shinyUI(fluidPage(
  titlePanel("Algorithm Backtester"),
  
  sidebarLayout(position = "left",
                sidebarPanel( 
                  h1("Parameters"),
                  fluidRow(
                    selectInput("model", 
                      label = "Choose a model",
                      choices = list("HMM"),
                      selected = "HMM"),
                    textInput("symbol", 
                      label = h3("Stock Symbol"), 
                      value = "^GSPC"),
                    dateInput("date", 
                      label = h3("Start date"),
                      value = "2010-04-04"),
                    sliderInput("window", 
                      label = "Backtest time window (trading days):",
                      min = 1, max = 100, value = 50),
                    sliderInput("time", 
                      label = "Backtest time period (trading days):",
                      min = 10, max = 252, value = 100),
                    radioButtons("states", 
                      label = h3("Number of States"),
                      choices = list("2" = 2, "3" = 3, "4" = 4),selected = 2),
                    actionButton("showDates",
                      label = h4("Show chosen dates")),
                    actionButton("showPrices",
                      label = h4("Show preview of price series"))
                  )
                ),
                mainPanel(
                  h1("Backtest"),
                  p("This application backtests an algorithm for a certain time period."),
                  textOutput("model"),
                  br(),
                  textOutput("trainingStart"),
                  textOutput("trainingEnd"),
                  textOutput("algoEnd"),
                  br(),
                  tableOutput("prices")
                  
                )
  )
))