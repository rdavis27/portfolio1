library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Portfolio"),
    
    # Sidebar with a slider input for the number of bins
    fluidRow(
        column(4, wellPanel(
            #fileInput("portfile", "File from which to create portfolio"),
            #textInput("portname", "New portfolio name", value=""),
            textInput("password", "Password", value="rpass"),
            selectInput("portfolio", "Portfolio", choices = "test"),
            checkboxInput("adjusted", "Adjust for Dividends", value = TRUE),
            checkboxInput("ordersym", "Order by Symbol", value = TRUE),
            checkboxInput("usecomma", "Use Commas", value = TRUE),
            dateRangeInput('dateRange',
                           label = 'Date range input: yyyy-mm-dd',
                           start = '2018-01-01', end = Sys.Date()),
            radioButtons("span", "Timespan:", inline = TRUE,
                         c("Use above dates","1M","3M","6M","YTD","1Y","2Y","5Y","10Y","MAX"), selected = "1Y")
        )),
        # Show a plot of the generated distribution
        column(8,
            tabsetPanel(
               tabPanel(
                   "Dollars",
                   verbatimTextOutput("dollars")
               ),
               tabPanel(
                   "Percent",
                   verbatimTextOutput("percent")
               ),
               tabPanel(
                   "Portfolio",
                   verbatimTextOutput("portfolio")
               ),
               tabPanel(
                   "Description",
                   verbatimTextOutput("description")
               )
            )
        )
    )
))