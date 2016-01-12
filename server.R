library(shiny)
library(datasets)
library(forecast)
library(zoo) 
library(xts) 
library(dygraphs)
library(lubridate)

shinyServer(function(input, output) {
  
  getDataset <- reactive({
    if (input$datasetvar=="AirPassengers")
    {
      return(AirPassengers)
    }
    else if (input$datasetvar=="gas")
    {
      return(gas)
    }
    else if(input$datasetvar=="wineind")
    {
      return(wineind)
    }
    else
    {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      csvInput <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                          quote=input$quote)
      csvInput[[input$dateColumnName]] <- as.Date(as.yearmon(csvInput[[input$dateColumnName]], input$dateFormat))
      
      strt <- first(csvInput[[input$dateColumnName]])
      strt <- c(year(strt), month(strt))      
      endTS <- last(csvInput[[input$dateColumnName]])
      endTS <- c(year(endTS), month(endTS))      
      
      x <- ts(csvInput[[input$dataColumnName]], start = strt, end = endTS, frequency = input$frequencyInput)
      return(x)
      
      x <- ts(x, start = strt, end = endTS, frequency = input$frequencyInput)
      
    }
  })
  
  output$caption <- renderText({
    paste("Dataset: ", input$datasetvar)
  })
  
  output$dcompPlot <- renderPlot({
    ds_ts <- ts(getDataset(), frequency=input$frequencyInput)
    f <- decompose(ds_ts)
    plot(f)
  })
  
  output$arimaForecast <- renderUI({
    dygraphOutput("arimaForecastPlot")
  })
  
  output$arimaForecastAccuracy <- renderUI({
    tags$div( class = "accuracyOut",
              tags$h2("Accuracy"),
              tableOutput("accuracyArima")
    )
  })
  
  output$arimaForecastPlot <- renderDygraph({
    fit <- auto.arima(getDataset())
    
    fitForecast <- forecast(fit, h=input$ahead)
    
    strt <- date_decimal(first(index(fitForecast$x)))
    strt <- c(year(strt), month(strt))
    
    endTS <- date_decimal(last(index(fitForecast$mean)))
    endTS <- c(year(endTS), month(endTS))
    
    lower <- fitForecast$mean
    lower[] <- fitForecast$lower[,2]
    upper <- fitForecast$mean
    upper[] <- fitForecast$upper[,2]
    
    fitMerged <- cbind(lwr=lower, predicted=c(fitForecast$fitted, fitForecast$mean), upr=upper, actual=fitForecast$x, deparse.level = 1)
    fitMerged <- ts(fitMerged, frequency = 12, start = strt, end = endTS)
    
    output$accuracyArima <- renderTable(accuracy(fitForecast))
    
    dygraph(fitMerged, main = fitForecast$method) %>%
      dySeries("actual", label = "Actual") %>%
      dySeries(c("lwr", "predicted", "upr"), label = "Predicted") %>%
      dyRangeSelector(height = 20)
  })
  
  output$epxSmoothing <- renderUI({
    dygraphOutput("etsPlot")
  })
  
  output$expSmoothingAccuracy <- renderUI({
    tags$div( class = "accuracyOut",
              tags$h2("Accuracy"),
              tableOutput("accuracyEts")
    )
  })
  
  output$etsPlot <-   renderDygraph({
    fit <- ets(getDataset())
    
    fitForecast <- forecast(fit, h=input$ahead)
    
    strt <- date_decimal(first(index(fitForecast$x)))
    strt <- c(year(strt), month(strt))
    
    endTS <- date_decimal(last(index(fitForecast$mean)))
    endTS <- c(year(endTS), month(endTS))
    
    lower <- fitForecast$mean
    lower[] <- fitForecast$lower[,2]
    
    upper <- fitForecast$mean
    upper[] <- fitForecast$upper[,2]
    fitMerged <- cbind(lwr=lower, predicted=c(fitForecast$fitted, fitForecast$mean), upr=upper, actual=fitForecast$x, deparse.level = 1)
   
    fitMerged <- ts(fitMerged, frequency = 12, start = strt, end = endTS)
    
    output$accuracyEts <- renderTable(accuracy(fitForecast))
    
    dygraph(fitMerged, main = fitForecast$method) %>%
      dySeries("actual", label = "Actual") %>%
      dySeries(c("lwr", "predicted", "upr"), label = "Predicted") %>%
      dyRangeSelector(height = 20)
  })
  
})