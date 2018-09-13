library(shiny)
library(googlesheets)
library(dplyr)
library(DT)


ui <- fluidPage(sidebarLayout(
  sidebarPanel("KID's HOME CHORES",
               htmlOutput("googlechoreForm"),
               actionButton("refresh", "Refresh Goggle sheet"), width = 4),
  mainPanel(    
    tabsetPanel(
      tabPanel("Historical Data", DT::dataTableOutput("dt_sheet"), id = 'dt_sheet_table'),
      tabPanel("Not Paid", DT::dataTableOutput("not_paid_sheet"), id = 'dt_sheet_table2')
    )
)
)
)




server <- function(input, output) {
  
  output$googlechoreForm <- renderUI({
    tags$iframe(id           = "choreForm",
                src          = "https://docs.google.com/forms/d/e/1FAIpQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width        = 350,
                height       = 600,
                frameborder  =   0,
                marginheight =  10)
  })

  output$dt_sheet = DT::renderDataTable({
    input$refresh
    sheet = gs_title("NEW")
    data  = sheet %>% gs_read(ws = "Form Responses 1")
    DT::datatable(data)
  })
  
  output$not_paid_sheet = DT::renderDataTable({
    input$refresh
    sheet = gs_title("NEW")
    not_paid  = sheet %>% gs_read(ws = "Form Responses 1") 
    not_paid = not_paid %>% filter(is.na(paid))
    DT::datatable(not_paid)
    
  })
}

shinyApp(ui = ui, server = server)
