library(d3heatmap)
library(officer)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(stringsAsFactors=FALSE)

server <- function(input, output, session)
{
	v <- reactiveValues()

	observeEvent(input$goButton,{
		print(input$file1$datapath)
		print("goButton")

		source("methods.R")
		parse_content(input$file1$datapath)
	})
	
	observeEvent(input$goButton,{
		source("methods.R")
	})
}