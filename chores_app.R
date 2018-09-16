library(shiny)
library(googlesheets)
library(dplyr)
library(DT)

ui = fluidPage(sidebarLayout(
  sidebarPanel("KID's HOME CHORES",
               htmlOutput("googlechoreForm"),
               actionButton("refresh", "Refresh Goggle sheet"), width = 4),
  mainPanel(    
    tabsetPanel(
      tabPanel("Historical Data", DT::dataTableOutput("dt_sheet"), id = 'dt_sheet_table'),
      tabPanel("Not Paid", 
               DT::dataTableOutput("not_paid_sheet"),
               DT::dataTableOutput("owned"),
               actionButton("Pay", "Pay"), width = 4, 
               id = 'dt_sheet_table2')
    )
  )
)
)

server = function(input, output) {
  # Get the spreadsheet from google
  sheet = function(){
    input$refresh
    sheet = gs_title("NEW")
  }
  # Get the chores data sheet - All of it
  historic = function(){
    historic  = sheet() %>% gs_read(ws = "Form Responses 1")
  }
 
  
 
   
  not_paid = function(){
    not_paid = historic()
    not_paid = not_paid %>% filter(is.na(paid)) %>% select_('Timestamp', 'Name', 'Chores', 'Points')
  }
  
  
  
  
  
  owned = function(){
    datap = not_paid() %>% group_by(Name) %>% summarise(Owned = sum(Points))
  }
  output$googlechoreForm <- renderUI({
    tags$iframe(id           = "choreForm",
                src          = "https://docs.google.com/forms/d/e/1FAIpQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width        = 350,
                height       = 600,
                frameborder  =   0,
                marginheight =  10)
  })
  output$dt_sheet = DT::renderDataTable({
    DT::datatable(historic(),  options = list(pageLength = 15, dom = 'tip'), rownames = FALSE)
  })
  output$not_paid_sheet = DT::renderDataTable({
    DT::datatable(not_paid(),  options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
  })
  output$owned = DT::renderDataTable({
    DT::datatable(owned(),  options = list(pageLength = 2, dom = 't'), rownames = FALSE)
  })  
}

shinyApp(ui = ui, server = server)