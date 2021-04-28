#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(digest)
library(DT)
library(tidyverse)

ledger <- tibble(
    Owner = "win",
    NFT_ID = "xzy",
    Timestamp = "sept 3030"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NFT Generator"),
    
    fluidRow(
        column(2, 
               fileInput(inputId = "file", label = "Upload NFT"),
               textInput(inputId = "owner", label = "Name", placeholder = "e.g. Win Cowger"),
               actionButton(inputId = "update_ledger", 
                            label   = "Update Ledger", 
                            class   = "btn-success")),
        column(10, 
               DT::dataTableOutput(outputId = 'ledger')
        ))
    
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    hashed_data <- reactive({
        req(input$file)
        digest(file = as.character(input$file$datapath))
    })

    
    df <- eventReactive(input$update_ledger, {
        if(input$owner != "" && input$update_ledger > 0){
            newrow = tibble(
                    Owner = input$owner,
                    NFT_ID = hashed_data(),
                    Timestamp = as.character(Sys.time()))
            ledger <<- rbind(ledger, newrow)
        }
        ledger
    }, ignoreNULL = FALSE)
    
    
    output$ledger <- renderDataTable(
        df()
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
