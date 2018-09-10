# Loading required libraries
library(googlesheets)
suppressMessages(library(dplyr))

# Check the list of sheets
my_sheets <- gs_ls()
# Listing the table of interest:
gs_ls("NEW House Chores")
# Getting the data from the sheet
# reading the file
gap <- gs_title("NEW House Chores")
# Checking the sheets in the spreadsheet
gs_ws_ls(gap)
# Getting the data from the responses
data = gap %>% gs_read(ws = "Form Responses 1")
# generating a date field.
data$Day = as.Date(data$Timestamp,"%m/%d/%Y")
# Adding the total number of point for each child. 
data%>%group_by(Name)%>%summarise(t_points = sum(Points))
data
#####

data %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.)))) -> f_NA
select_if(function(f) any(is.na(f)))

arfard(GAP_KEY <- gs_gap_key())

third_party_gap1 <- GAP_KEY %>%
  gs_key()
c
gs_ws_ls(gap)

f = gap %>% gs_read(ws = "Form Responses 1")
gap %>% gs_edit_cells(ws = "Form Responses 1", input = 'Y')
gs_edit_cells(gap, ws = "Form Responses 1", input = "SAdasd", anchor = "D2", byrow = FALSE,
               col_names = FALSE, trim = FALSE, verbose = TRUE)
gap
for (i in 3:6) {
  foo <- gap %>% gs_edit_cells(ws = "Form Responses 1", input = 'N')
  Sys.sleep(0.3)
}
