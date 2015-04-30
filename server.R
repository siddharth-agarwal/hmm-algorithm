# server.R
library(quantmod)
library(depmixS4)
library(ggplot2)
source("func.r")
shinyServer(
  function(input, output) {
    output$model <- renderText({
      paste("Model: ", input$model, "with", input$states, "states")
    })
    
    output$trainingStart <- renderText({
      if (input$showDates){
        paste("Training start date: ", input$date)
      }
    })
    
    output$trainingEnd <- renderText({
      if (input$showDates){
        training = NextTradingDate(input$date,input$window)
        paste("Training end date: ", training)
      }
    })
    
    output$algoEnd <- renderText({
      if (input$showDates){
        end = NextTradingDate(input$date,input$time)
        paste("Algo end date: ", end)
      }
    })
    
    dataInput <- reactive({
      GetData(input$symbol, input$date)
    })
    
    dataReturns <- reactive({
      data = data.frame(time= list(input$time), returns.time= numeric(input$time), benchmark.time = numeric(input$time))
      print(dimnames(data))
      dimnames(data)[[2]] <- c("time", "returns", "benchmark")
      print(dimnames(data))
      data = runModel(dataInput(), as.integer(input$states), as.Date(input$date), NextTradingDate(input$date,input$window), as.integer(input$time))
      dimnames(data)[[2]] <- c("time", "returns", "benchmark")
      return(data)
    })
    
    output$prices <- renderDataTable({
      if (input$showPrices){
        data = dataInput()
        head(data)
      }
    })
    
    output$returns <- renderDataTable({
      if (input$showBacktest){
        data2 = data.frame(time= list(input$time), returns.time= numeric(input$time), benchmark.time = numeric(input$time))
        data2 = dataReturns()
        dimnames(data2)[[2]] <- c("time", "returns", "benchmark")
        return(data2)
      }
    })
    
    output$plot <- renderPlot({
      
      if (input$showBacktest){
        ggplot(dataReturns(), aes(as.Date(time))) + 
          geom_line(aes(y = benchmark, colour = "benchmark")) + 
          geom_line(aes(y = returns, colour = "returns"))
      }
    })
})