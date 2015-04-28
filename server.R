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
    
    output$prices <- renderTable({
      if (input$showPrices){
        source("func.r")
        start.date = as.Date(input$date)
        data = GetData(input$symbol,input$date)
        head(data)
      }
    })
})