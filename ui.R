options(stringsAsFactors=FALSE)

library(corrplot)
library(d3heatmap)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(shinythemes)

options(stringsAsFactors=FALSE)
options(shiny.maxRequestSize = 100*1024^2)

#rm("d1"); rm("d2"); rm("d3")

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
			actionButton("goButton", "Analyse dataset!")),
		tabPanel("Density plot",
			fileInput("file3", "Insert the file", multiple = TRUE, accept = c(".tsv")),
			actionButton("goButton2", "Analyse dataset!")),
		tabPanel("Correlation plots",
			fileInput("file4", "Insert the file ('eigengenes_WCGNA_31aug18')", multiple = TRUE, accept = c(".txt")),
			actionButton("goButton3", "Analyse dataset!"))
		)),
		mainPanel(
			useShinyjs(),
			plotOutput(outputId = "plot")
		)
	)
)