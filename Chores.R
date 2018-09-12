# Loading required libraries

library(googlesheets)
suppressMessages(library(dplyr))
# Check the list of sheets
my_sheets <- gs_ls()
# Listing the table of interest:
gs_ls("NEW")
# Getting the data from the sheet
# reading the file
gap <- gs_title("NEW")
# Checking the sheets in the spreadsheet
gs_ws_ls(gap)
# Getting the data from the responses
data = gap %>% gs_read(ws = "Form Responses 1")
total_rows = nrow(data)
# generating a date field.
data$Day = as.Date(data$Timestamp,"%m/%d/%Y")
# NON PAID DATA
ndata = data%>%filter(is.na(paid))
n_rows = nrow(ndata)
if (total_rows == n_rows){n_rows = n_rows-1}

datap = ndata%>%group_by(Name)%>%summarise(t_points = sum(Points))

if (length(gs_ws_ls(gap)) == 1){
  # Create a new ws. 
  # NOT NEEDED AFTER THE FIRST
  gap <- gap %>% 
    gs_ws_new(ws_title = "paid", input = datap,
              trim = TRUE, verbose = FALSE)
} else {# Adding the total number of point for each child.
  datap$paid_on = Sys.Date()
  gs_add_row(gap, ws = 'paid', input = datap, verbose = TRUE)
}

# Adding the PAID flag to items paid today
for (i in (total_rows - n_rows + 1) : (total_rows+1)) {
  anchor_range = paste("R", i,"C5", sep = "", collapse = NULL)
  gap %>% gs_edit_cells(ws = "Form Responses 1", input = Sys.Date(), anchor = anchor_range)
  Sys.sleep(0.1)
}
