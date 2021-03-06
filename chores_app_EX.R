"
This app interacts with a google form and its linked google sheet to keep track of the chores
two (or more) kids perform at home and their point payment system.
It works entirely on R using a shiny. The working version can be found in:

This is mock-up version.
To use it, download the code, run it. Shiny will connect asked you to connect your google account. 
Once that is done and you have a form with a linked account, change the form's link and the
sheet's name below and that should be it. 
"
## Loading the required libraries
rm(list = ls()) 
library(shiny)        # App html connectivity
library(googlesheets) # googlesheets interface
library(dplyr)        # Data Manipulation
library(DT)           # Data tables - I used it more for presentation  (optional)
library(RColorBrewer) # Generate the colors for the pie charts
library(plotrix)      # Pie chart 

ui = fluidPage(
  # Generate a slide panel and a panel with three tabs. 
  # The side panel is for the form and the tabs for the data analytics
  titlePanel("Home chores tracker _EXAMPLE_"),
  sidebarLayout(
    sidebarPanel(
               htmlOutput("googlechoreForm"),
               actionButton("refresh", "Refresh Goggle sheet"), width = 4),
    mainPanel(
      
      tabsetPanel(
        tabPanel("CHARTS",
                 uiOutput("pie_charts"), id = 'charts'),
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
  # Renders the google form (It can be filled here too). 
  output$googlechoreForm <- renderUI({
    # Links google form, Just change the link in src field to point to the approppiate form
    tags$iframe(id           = "choreForm",
                src          = "https://docs.google.com/forms/d/e/1FAIpQLSeo4BsB4IqBYP-FrqKCCK_Yu0OsOXY5kbRxVZbl84ehdL3rjw/viewform?embedded=true",
                width        = 350,
                height       = 600,
                frameborder  =   0,
                marginheight =  10)
  })
  change <- reactive({
    # Reacts to the submit button. 
     paste(input$refresh , input$payments)
  })
  # Data manipulation
  data = eventReactive(change(),{
    # This function connects to the sheet and generates the data for the tables and pie charts
    sheet    = googlesheets::gs_title("NEW") # Change the name of the sheet of interets here
    historic = sheet %>% googlesheets::gs_read(ws = "Form Responses 1") # Change the spreadsheet name
    ndata    = historic %>% filter(is.na(paid))
    # Calculate lenghts of the dataframe - Necesary to update the form later. 
    total_rows = nrow(historic) 
    n_rows = nrow(historic %>% filter(is.na(paid)==FALSE))
    # Unpaid data
    not_paid = ndata %>% select_('Timestamp', 'Name', 'Chores', 'Points')
    owned    = not_paid %>% group_by(Name) %>% summarise(Owned = sum(Points)) 
    # Color pallete for the pies
    pals = RColorBrewer::brewer.pal(nrow(table(historic$Chores)), 'Paired')
    paleta = setNames(pals, unique(historic$Chores))
    # DataFramed for the pies. (Change names)
    eu_data = historic %>% filter(Name == 'Eusebio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()) %>% arrange(desc(Chores))
    an_data = historic %>% filter(Name == 'Antonio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n()) %>% arrange(desc(Chores))
    # Returns a list of objects. 
    return(list(historic = historic, not_paid = not_paid, owned = owned,
                eu_data = eu_data, an_data = an_data, pale = paleta,
                total_rows = total_rows, n_rows = n_rows))
  })
  #  Tables
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
  # Plots (Pie Charts)
  output$pie_charts <- renderUI({
    x <- list(
      renderPlot({
       plotrix::pie3D(data()$eu_data$t_points, 
            labels   = data()$eu_data$Chores, 
            main     = "Eusebio",
            theta    = 1, 
            explode  = 0.15, 
            radius   = 1, 
            labelcex = 1,
            col      = data()$pale,
            start    = 1,
            col.main = 'red'
            )
    }),
    renderPlot({
       plotrix::pie3D(data()$an_data$t_points, 
            labels   = data()$an_data$Chores, 
            main     = "Antonio",
            theta    = 1, 
            explode  = 0.15, 
            radius   = 1, 
            labelcex = 1,  
            col      = data()$pale,
            start    = 1,
            col.main = 'blue'
            )
    })
    )
  fluidRow(
    # Allow to render the plots next to each other 
    lapply(
      X = split(x, f = rep(c(1, 2), length.out = length(x))),
      FUN = column, width = 6
    )
  )
}
)
}

shinyApp(ui = ui, server = server)