library(shiny)
library(googlesheets)
library(dplyr)
library(DT)


library(paletteer)
library(waffle)
library(cartography)

library(plotrix)

ui = fluidPage(
  titlePanel("My Application"),
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
                 DT::dataTableOutput("not_paid_sheet"),
                 DT::dataTableOutput("owned"),
                 actionButton("payments", label = "EXECUTE PAYMENTS"),
                 #width = 4,
                 id = 'dt_sheet_table2'),
        tabPanel("Historical Data", DT::dataTableOutput("dt_sheet"), id = 'dt_sheet_table')
        )
      )
    )
)

server = function(input, output) {
  # Renders the form. it can be filled here too. 
  output$googlechoreForm <- renderUI({
    tags$iframe(id           = "choreForm",
                src          = "https://docs.google.com/forms/d/e/1FAIpQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width        = 350,
                height       = 600,
                frameborder  =   0,
                marginheight =  10)
  })
  # Get the gsheet
  sheet = reactive({
    input$payments
    input$refresh
    gs_title("NEW")
  })
  
  # Get the chores data sheet - All of it
  historic = reactive({
    sheet() %>% gs_read(ws = "Form Responses 1")
  })
  # Filters the DT to show only the values that have not been paid
  not_paid = reactive({
    historic() %>% filter(is.na(paid)) %>% select_('Timestamp', 'Name', 'Chores', 'Points')
  })
  # Sums the values that have not been paid
  owned = reactive({
    not_paid() %>% group_by(Name) %>% summarise(Owned = sum(Points))
  })
  # TABLES
  output$dt_sheet = DT::renderDataTable({
    DT::datatable(historic(),  options = list(pageLength = 15, dom = 'tip'), rownames = FALSE)
  })
  output$not_paid_sheet = DT::renderDataTable({
    DT::datatable(not_paid(),  options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
  })
  output$owned = DT::renderDataTable({
    DT::datatable(owned(),  options = list(pageLength = 2, dom = 't'), rownames = FALSE)
  })
  observeEvent(input$payments, {
    data = historic()
    total_rows = nrow(data)
    ndata = data %>% filter(is.na(paid))
    n_rows = nrow(ndata)
    if (total_rows == n_rows)
      {print('no updates')}
    else{
      for (i in (total_rows - n_rows) : (total_rows+1)) {
         anchor_range = paste("R", i, "C5", sep = "", collapse = NULL)
         sheet() %>% gs_edit_cells(ws = "Form Responses 1", input = Sys.Date(), anchor = anchor_range)
    }
  }
  })
  output$sumplot = renderPlot(
    pie3D((historic() %>% filter(Name == 'Eusebio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()))$t_points, 
          labels = (historic() %>% filter(Name == 'Eusebio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()))$Chores, 
          main = "Eusebio",
          theta=pi/4, explode = 0.2, 
          radius= 1, 
          labelcex = 1,  start= 0 )

  )
  output$sumplot1 = renderPlot(
    pie3D((historic() %>% filter(Name == 'Antonio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()))$t_points, 
          labels = (historic() %>% filter(Name == 'Antonio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()))$Chores, 
          main = "Antonio",
          theta=pi/4, explode = 0.2, 
          radius= 1, 
          labelcex = 1,  start= 0 )
    
  )
}
shinyApp(ui = ui, server = server)