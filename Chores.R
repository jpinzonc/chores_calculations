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
#data$Day = as.Date(data$Timestamp,"%m/%d/%Y")
# NON PAID DATA
ndata = data%>%filter(is.na(paid))
n_rows = nrow(ndata)
if (total_rows == n_rows){n_rows = n_rows-1}


#OWNED CALCULATIONS
datap = ndata%>%group_by(Name)%>%summarise(t_points = sum(Points))

if (gs_ws_ls(gap)[2] != 'paid'){
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


eu_data = data %>% filter(Name == 'Eusebio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n())
an_data = data %>% filter(Name == 'Antonio') %>% select_('Chores', 'Points')%>%group_by(Chores)%>%summarise(t_points = n())

library(ggplot2)

bp= ggplot(eu_data, aes(x="", y=t_points, fill=Chores)) + geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)
bp + scale_fill_brewer(palette="Blues")+
  theme_minimal()


library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

bp + scale_fill_brewer("Blues") +   blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = t_points/ nrow(eu_data) + c(0, cumsum(t_points)[-length(t_points)]), 
                label = (t_points)), size=5
            )


library(plotrix)

pie3D(eu_data$t_points, labels = eu_data$Chores, main = "Points per chore", 
      explode = 0.2, radius= 0.9, labelcex = 1,  start= 0 )


library(paletteer)
library(waffle)
library(cartography)
colors =  paletteer_dynamic(cartography, blue.pal, 15)
iron(
  waffle(eu_data, title = "Eusebios", rows = 5, size = 1, legend_pos = "bottom", color = colors),
  waffle(an_data, title = "Antonios", rows = 5, size = 1, legend_pos = "bottom", color = colors)
)
