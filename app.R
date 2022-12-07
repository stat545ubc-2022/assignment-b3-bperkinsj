library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(stringr)

# First added feature: I used the DataTable package to create an
# interactive table output, allowing the user to sort the results 
# as they desire. The modifications necessary for this are under
# the comments "Feature 1".
# Second added feature: I added an output that shows the user how
# many drinks fit their specified restrictions. This gives the user
# an idea of the drink availability without having to scroll through
# the entire table. Modifications necessary for this are under the 
# comments "Feature 2".
# Third added feature: I added a download button that lets the user 
# download their filtered table. Modifications necessary for this
# are under the comments 'Feature 3'. 

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      "Find your drink selections!",
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      selectInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
# Feature 2
      textOutput('totaldrinks'),
      br(), br(),
# Feature 1
      DT::dataTableOutput("results"),
      br(), br(),
# Feature 3
      downloadButton('downloadData', 'Download')
    )
  )
)

server <- function(input, output) {
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