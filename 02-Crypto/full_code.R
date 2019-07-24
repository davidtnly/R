library(rsconnect)
library(xts)
library(dygraphs)
library(coindeskr) 
library(shiny) 

data <- read.csv("data.csv", stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
options(scipen = 000)
str(data)

ui <- shinyUI(
  fluidPage(
    # Create a title
    titlePanel('Cryptocurrency Time Series'),
    # Create a sidebar
    sidebarLayout(
      sidebarPanel(
        # Add choices
        selectInput('selectCoins', # unique inputID
                    'Select Coin(s)', # label
                    choices = c("bitcoin", "ethereum", "litecoin"), # options
                    selected = c("ethereum", "litecoin"),
                    multiple = T), # allows only one but set to T for multiple choices
        # Add marketcaps
        selectInput('selectOutput',
                    'Select Output',
                    choices = c(Price = "price", 'Market Cap' = "market")),
        # Add date range
        dateRangeInput('selectDate',
                       'Select Date',
                       start = min(data$Date),
                       end = max(data$Date))
      ),
      # Create a main panel
      mainPanel(
        mainPanel(div(dygraphOutput("priceGraph", width = "150%", height = "800px")))
      )
    )))

server <- function(input, output) { # both arguments are lists
  
  # Build data (get inputs, filter data, return data)
  getData <- reactive({
    # step 1: get inputs
    selectedCoins <- input$selectCoins
    selectedOutput <- input$selectOutput
    dates <- input$selectDate
    startDate <- dates[1]
    endDate <- dates[2]
    
    # step 2: filter data
    cryptoColumns <- colnames(data)
    coinColumns <- cryptoColumns[cryptoColumns %in% paste0(selectedOutput, selectedCoins)]
    selectedColumns <- append("Date", coinColumns)
    newdata <- data[data$Date >= startDate & data$Date <= endDate, selectedColumns, drop = FALSE]
    
    # step 3: format and return data
    if(selectedOutput == "market") {
      newdata[coinColumns] <- lapply(newdata[coinColumns], FUN = function(x) x/1000000000)
    }
    colnames(newdata) <- gsub(selectedOutput, "", colnames(newdata))
    newdata
  })
  
  # create the interactive chart using dygraph
  output$priceGraph <- renderDygraph({
    newdata <- getData()
    time_series <- xts(newdata, order.by = newdata$Date)
    dygraph(time_series)
  })
}



shinyApp(ui, server)