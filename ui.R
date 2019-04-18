options(stringsAsFactors=FALSE)

library(d3heatmap)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(stringsAsFactors=FALSE)
options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(  
tags$head(
	tags$style(HTML("
	.shiny-output-error {
	visibility: hidden;
}
body {
	#background-color: #23443333;
}
body, label, input, button, select { 
	font-family: 'Arial';
}"))
  ), 
  theme = shinytheme("sandstone"),  useShinyjs(), useShinyalert(), 
	sidebarLayout(
		sidebarPanel(
		tabsetPanel(id = "tabset",
		tabPanel("WGCNA analysis data",
			fileInput("file1", "Insert the file", multiple = TRUE, accept = c(".txt")),
			fileInput("file2", "Insert the file", multiple = TRUE, accept = c(".tsv")),
			actionButton("goButton", "Analyse dataset!"))
		)),
		mainPanel(
			useShinyjs(),
			plotOutput(outputId = "plot")
		)
	)
)