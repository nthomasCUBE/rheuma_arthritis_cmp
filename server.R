options(stringsAsFactors=FALSE)

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
		parse_content(input$file1$datapath,input$file2$datapath)
	})
	
	observeEvent(input$goButton2,{
		print(c("density plot analysis"))
		source("methods.R")
		output$plot=renderPlot({
			density_plot_analysis(input$file3$datapath)
		});
	})
}