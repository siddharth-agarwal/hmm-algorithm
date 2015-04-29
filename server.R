# server.R
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
      if (input$showBacktest){
        data = runModel(dataInput(), as.integer(input$states), as.Date(input$date), NextTradingDate(input$date,input$window), as.integer(input$time))
      }
    })
    
    output$prices <- renderTable({
      if (input$showPrices){
        data = dataInput()
        head(data)
      }
    })
    
    output$returns <- renderDataTable({
      dataReturns()
    })
    
    output$plot <- renderPlot({
      ggplot(dataReturns(), aes(as.Date(X20L))) + 
        geom_line(aes(y = benchmark.time, colour = "benchmark")) + 
        geom_line(aes(y = returns.time, colour = "returns"))
    })
})