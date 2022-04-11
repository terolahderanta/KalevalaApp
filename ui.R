#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("KalevalApp"),

    tabsetPanel(
      tabPanel("Etsi Kalevalasta",
               
               sidebarLayout(
                 sidebarPanel(
                   textInput(inputId = "etsittava", 
                             label = "Etsi", 
                             value = ""),
                   actionButton("etsi", "Etsi tästä"),
                   tableOutput("tulokset")
                 ),
                 
                 mainPanel(
                   
                 )
               )),
      
      tabPanel("Kalevalan naiset ja miehet",
               
               sidebarLayout(
                 sidebarPanel(),
                 mainPanel()
               ))
    )
    
    
))
