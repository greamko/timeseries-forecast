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
                          quote=input$quote, dec=input$dec)
      csvInput[[input$dateColumnName]] <- as.Date(as.yearmon(csvInput[[input$dateColumnName]], input$dateFormat))

            
      strt <- first(csvInput[[input$dateColumnName]])
      strt <- c(year(strt), month(strt))      
      endTS <- last(csvInput[[input$dateColumnName]])
      endTS <- c(year(endTS), month(endTS))      
      
      frequency = input$frequency
      if(frequency == 0){
        frequency = findfrequency(csvInput[[input$dataColumnName]])
      }

      #x <- ts(csvInput[[input$dataColumnName]], start = strt, frequency = findfrequency(csvInput[[input$dataColumnName]]))
      x <- ts(csvInput[[input$dataColumnName]], start = strt, frequency = frequency)
      return(x)
      #return(csvInput[[input$dataColumnName]])
      #x   <- ts(x, start = strt, end = endTS, frequency = input$frequencyInput)
      
    }
  })
  
  getModel <- reactive({
    dataSet <- getDataset()
    if(input$forecastMethod == 'arima'){
      return (auto.arima(dataSet));
    }
    else if(input$forecastMethod == 'ets'){
      return (ets(dataSet));
    }
    else if(input$forecastMethod == 'hw'){
      return (HoltWinters(dataSet));
    }
    else if(input$forecastMethod == 'croston'){
      return (croston(dataSet));
    }
    else if(input$forecastMethod == 'nnetar'){
      return (nnetar(dataSet));
    }
    else if(input$forecastMethod == 'splinef'){
      return (splinef(dataSet));
    }
    else if(input$forecastMethod == 'rwf'){
      return (rwf(dataSet));
    }
    else if(input$forecastMethod == 'thetaf'){
      return (thetaf(dataSet));
    }
    else if(input$forecastMethod == 'tbats'){
      return (tbats(dataSet));
    }
  })
  
  getTestModel <- function(testData, modelIn){
    if(input$forecastMethod == 'arima'){
      return (Arima(testData, model = modelIn));
    }
    else if(input$forecastMethod == 'ets'){
      return (ets(testData, model = modelIn));
    }
    else if(input$forecastMethod == 'hw'){
      return (HoltWinters(testData, alpha = modelIn$alpha, beta = modelIn$beta, gamma = modelIn$gamma, seasonal = modelIn$seasonal));
    }
    else if(input$forecastMethod == 'nnetar'){
      return (nnetar(testData, p = modelIn$p, P = modelIn$P, size = modelIn$size));
    }
    else if(input$forecastMethod == 'splinef'){
      return (splinef(testData, level = modelIn$level, method = modelIn$method));
    }
    else if(input$forecastMethod == 'thetaf'){
      return (thetaf(testData, level = modelIn$level));
    }
    else if(input$forecastMethod == 'tbats'){
      return (tbats(testData, use.box.cox = modelIn$parameters$control$use.box.cox));
    }
  }
  
  output$caption <- renderText({
    paste("Dataset: ", input$datasetvar)
  })
  
  output$dcompPlot <- renderPlot({
    frequency = input$frequency
    if(frequency == 0){
      frequency = findfrequency(getDataset())
    }
    ds_ts <- ts(getDataset(), frequency=frequency)
    f <- decompose(ds_ts)
    plot(f)
  })
  
  output$forecastWindow <- renderUI({
    dygraphOutput("forecastPlot")
  })
  
  output$forecastWindowAccuracyOut <- renderUI({
    tags$div( class = "accuracyOut",
              tags$h2("Accuracy"),
              tableOutput("accuracy")
    )
  })
  
  output$forecastPlot <-   renderDygraph({
    dataSet <- getDataset()
    fit <- getModel();
    
    fitForecast <- forecast(fit, h=input$ahead)
    
    modelLength <- length(dataSet)
    
    accuracyTestStart <- modelLength - round(modelLength / 10)
    testFit <- getTestModel(testData = window(dataSet, end = index(dataSet)[accuracyTestStart-1]), modelIn = fit)
    testForecast <- forecast(testFit, h=round(modelLength / 10))
    
    output$accuracy <- renderTable(accuracy(testForecast, window(dataSet, start = index(dataSet)[accuracyTestStart])))
    
    strt <- date_decimal(first(index(fitForecast$x)))
    strt <- c(year(strt), month(strt))
    
    endTS <- date_decimal(last(index(fitForecast$mean)))
    endTS <- c(year(endTS), month(endTS))
    
    lower <- fitForecast$mean
    lower[] <- fitForecast$lower[,2]
    
    upper <- fitForecast$mean
    upper[] <- fitForecast$upper[,2]
    fitMerged <- cbind(lwr=lower, predicted=c(fitForecast$fitted, fitForecast$mean), upr=upper, actual=fitForecast$x, deparse.level = 1)
   
    fitMerged <- ts(fitMerged, frequency = 12, start = strt)
    
    dygraph(fitMerged, main = fitForecast$method) %>%
      dySeries("actual", label = "Actual") %>%
      dySeries(c("lwr", "predicted", "upr"), label = "Predicted") %>%
      dyRangeSelector(height = 20)
  })
  
})