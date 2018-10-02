# Chores Calculation app integrated with Google sheets and Google forms

## PURPOSE:

To integrate a google sheet and form to track home chores at home

## DESCRIPTION

Recently my wife and I decided to implement a chore system to our kids. Basically we give them points for each chore they finish at home and at the end a period (e.g. week) each one gets a certain amount of money. The amount is based on the points they make and requires keeping track of their activities over the period. At this point my data scientists side said: you should write an app for that! Since I have not given a try to R in a while, I decided to make the app with Shiny apps on RStudio. 

The points for each kid are recorded using a google form and the app access both the form, to submit new tasks, and the associated spreadsheet, to analyze the data. The accessed information is then used to generate a table of unpaid chores and provide an interface to markdown the paid chores. 

The result is a nice app that can be used to keep track the actitivy of the kids at home.

AND you know that saying" *'happy wife, happy life'*. 

## RUNNING THE APP
To run the app:

	- Clone/Download the code from here. 

	- Edit the code to point to your form and sheet.

	- Run the app using RStudio directly

	- **OR** run it in the command line with:
		'Rscript -e 'library(methods); shiny::runApp("my_shiny/", launch.browser=TRUE)'

	- **OR** upload it to the Shiny apps (https://www.shinyapps.io/) and load it from anywhere in the world. 
		
		This example can be accessed at: 
			https://drdata.shinyapps.io/CHORES_EXAMPLE/

