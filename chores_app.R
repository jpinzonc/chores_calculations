library(shiny)
library(googlesheets)

server <- function(input, output) {
  
  output$googleForm <- renderUI({
    tags$iframe(id = "googleform",
                src = "https://docs.google.com/forms/d/e/1FAI12pQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width = 400,
                height = 625,
                frameborder = 0,
                marginheight = 0)
  })

  output$table <- renderDataTable({
    input$refresh
    sheet <- gs_title("NEW")
    data = sheet %>% gs_read(ws = "Form Responses 1")
    data
  })
}


ui <- fluidPage(sidebarLayout(
                  sidebarPanel("KID's HOME CHORES",
                               htmlOutput("googleForm"),
                               actionButton("refresh", "Refresh Sheet")),
                  mainPanel(dataTableOutput('table'))
)
)

shinyApp(ui = ui, server = server)
