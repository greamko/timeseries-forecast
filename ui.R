library(shiny)
library(dygraphs)

# Define UI 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Timeseries Forecasting"),
  
  # Sidebar with controls
  sidebarPanel(
    selectInput("datasetvar", "Variable:",
                list("Air Passengers" = "AirPassengers", 
                     "Australian total wine sales" = "wineind",
                     "Australian monthly gas production" = "gas",
                     "Own" = "own")),
    
    selectInput("forecastMethod", "Forecast Method:",
                list("Arima" = "arima", 
                     "Ets" = "ets",
                     "Holt Winters" = "hw",
                     "Nnetar" = "nnetar",
                     "splinef" = "splinef",
                     "thetaf" = "thetaf",
                     "Tbats" = "tbats")),
    
    numericInput("ahead", "Period to Forecast Ahead:", 12),

    numericInput("frequency", "Frequency:", 0),
    
    submitButton("Update View"),
    
    tags$hr(),
    
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    checkboxInput('header', 'Header', TRUE),
    
    
    textInput("dataColumnName", "Data column name:", "value"),
    
    textInput("dateColumnName", "Date column name:", "date"),
    
    textInput("dateFormat", "Date format:", "%d-%m-%Y"),
    
    tags$hr(),
    div(HTML('<table width="90%" border="0">
                  <tbody><tr>
         <td><strong>Symbol</strong></td>
         <td><strong>Meaning</strong></td>
         <td><strong>Example</strong></td>
         </tr>
         <tr>
         <td><strong>%d</strong></td>
         <td>day as a number (0-31) </td>
         <td>01-31</td>
         </tr>
         <tr>
         <td><strong>%a<br>
         %A</strong></td>
         <td>abbreviated weekday <br>
         unabbreviated weekday </td>
         <td>Mon<br>
         Monday</td>
         </tr>
         <tr>
         <td><strong>%m</strong></td>
         <td>month (00-12) </td>
         <td>00-12</td>
         </tr>
         <tr>
         <td><strong>%b<br>
         %B</strong></td>
         <td>abbreviated month<br>
         unabbreviated month </td>
         <td>Jan<br>
         January</td>
         </tr>
         <tr>
         <td><strong>%y<br>
         %Y</strong></td>
         <td>2-digit year <br>
         4-digit year </td>
         <td>07<br>
         2007</td>
         </tr>
         </tbody></table>')),
    tags$hr(),
    
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    
    radioButtons('dec', 'Decimal Mark',
                 c(Comma=',',
                   'Dot'='.'
                   ),
                 '.'),
    
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    
    uiOutput("uploadControl"),
    
    
    submitButton("Update View")
  ),
  
  
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Forecast", fluidRow(uiOutput("forecastWindow"), uiOutput("forecastWindowAccuracyIn"), uiOutput("forecastWindowAccuracyOut"))),
      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
    ),
    
    HTML(
      "<style>
      .accuracyOut{ text-align: center; margin:auto; display: table}
      </style>"
    )
  )
))