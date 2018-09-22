rm(list = ls()) 
library(shiny)
library(googlesheets)
library(dplyr)
library(DT)
library(RColorBrewer)
library(plotrix)

ui = fluidPage(
  titlePanel("Home chores tracker"),
  sidebarLayout(
    sidebarPanel(
               htmlOutput("googlechoreForm"),
               actionButton("refresh", "Refresh Goggle sheet"), width = 4),
    mainPanel(
      
      tabsetPanel(
        tabPanel("PLOT", 
                 plotOutput("sumplot"),
                 plotOutput("sumplot1"),
                 id = 'plot'),
        tabPanel("PAYMENTS", 
                 hr(tags$strong('UNPAID CHORES')),
                 DT::dataTableOutput("not_paid_sheet"),
                 hr(tags$strong('TOTAL OWNED')),
                 DT::dataTableOutput("owned"),
                 hr(),
                 div(style="display:inline-block;width:60%;text-align: center;"
                     ,actionButton("payments", label = "MAKE PAYMENT")),

                 id = 'dt_sheet_table2'),
        tabPanel("Historical Data", DT::dataTableOutput("dt_sheet"), id = 'dt_sheet_table')
        )
      )
    )
)

server = function(input, output) {
  # Renders the google form (It can be filled there too). 
  output$googlechoreForm <- renderUI({
     tags$iframe(id           = "choreForm",
                src          = "https://docs.google.com/forms/d/e/1FAIpQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width        = 350,
                height       = 600,
                frameborder  =   0,
                marginheight =  10)
  })
  change <- reactive({
     paste(input$refresh , input$payments)
  })
  
  # Generating the data for the app
  data = eventReactive(change(),{
     sheet    = googlesheets::gs_title("NEW")
     historic = sheet %>% googlesheets::gs_read(ws = "Form Responses 1")
     ndata    = historic %>% filter(is.na(paid))

     total_rows = nrow(historic) 
     n_rows = nrow(historic %>% filter(is.na(paid)==FALSE))

     not_paid = ndata %>% select_('Timestamp', 'Name', 'Chores', 'Points')
     owned    = not_paid %>% group_by(Name) %>% summarise(Owned = sum(Points)) 
     
     pals = RColorBrewer::brewer.pal(nrow(table(historic$Chores)), 'Paired')
     paleta = setNames(pals, unique(historic$Chores))

     eu_data = historic %>% filter(Name == 'Eusebio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n())
     an_data = historic %>% filter(Name == 'Antonio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n())

     return(list(sheet = sheet, historic = historic, 
                 not_paid = not_paid, owned = owned,
                 eu_data = eu_data, an_data = an_data, 
                 pale = paleta, total_rows = total_rows, n_rows = n_rows))
     })
  # Generating the tables
  output$dt_sheet = DT::renderDataTable({
     DT::datatable(data()$historic, options = list(pageLength = 15, dom = 'tip'), rownames = FALSE)
  })
  output$not_paid_sheet = DT::renderDataTable({
     DT::datatable(data()$not_paid,  options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
  })
  output$owned = DT::renderDataTable({
     DT::datatable(data()$owned,  options = list(pageLength = 2, dom = 't'), rownames = FALSE)
  })
  observeEvent(input$payments, {
     if (data()$total_rows == data()$n_rows) {cat('no updates')}
     else{
       for (i in (data()$n_rows+2): (data()$total_rows+1)) {
         anchor_range = paste("R", i, "C5", sep = "", collapse = NULL)
         data()$sheet %>% googlesheets::gs_edit_cells(ws = "Form Responses 1", input = Sys.Date(), anchor = anchor_range)
    }
  }
  })
  output$sumplot = renderPlot({
     plotrix::pie3D(data()$eu_data$t_points, 
          labels  = data()$eu_data$Chores, 
          main    = "Eusebio",
          theta   = pi/4, 
          explode = 0.2, 
          radius  = 1, 
          labelcex = 1,
          col = data()$pale,
          start   = 0 
          )
  })
  output$sumplot1 = renderPlot({
     plotrix::pie3D(data()$an_data$t_points, 
          labels  = data()$an_data$Chores, 
          main    = "Antonio",
          theta   = pi/4, 
          explode = 0.2, 
          radius  = 1, 
          labelcex = 1,  
          col     = data()$pale,
          start   = 0 
          )
  })
}
shinyApp(ui = ui, server = server)