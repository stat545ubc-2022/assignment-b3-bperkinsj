library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)

# This app contains 3 inputs and 3 outputs. The user inputs the price range,
# type of drink, and country of origin. The app outputs a plot of the 
# appropriate drinks, a short sentence saying how many drinks fit the 
# specifications, and a table containing the information for the appropriate
# drinks. The user has the option to download this table.

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      "Find your drink selections!",
      
      # Choose price range
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      
      # Choose type of drink
      selectInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      
      # Outputs country choice
      uiOutput("countryOutput")
    ),
    mainPanel(
      h3('Drink Selection', align='center'),
      
      # Drink selection plot
      plotOutput("coolplot"),
      br(), br(),

      # Total number of drinks found
      h4(textOutput('totaldrinks'), align='center'),
      br(), br(),

      # Results table
      DT::dataTableOutput("results"),
      br(), br(),

      # Button to download results table as a csv file
      downloadButton('downloadData', 'Download Selection')
    )
  )
)

server <- function(input, output) {
  
  # Choose country of origin
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
# Feature 2
  output$totaldrinks <- renderText({
    paste('Total number of bottles of ', tolower(input$typeInput),
          ' between $', input$priceInput[1], ' and $', input$priceInput[2],
          ' from ', str_to_title(input$countryInput), ':', nrow(filtered()))
  })
# Feature 1
  output$results <- DT::renderDataTable({
    filtered()
  })
# Feature 3
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('drink_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
}

shinyApp(ui = ui, server = server)