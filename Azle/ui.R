#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    sidebarPanel(radioButtons('Type', label = 'GI Type',
                                    choices = unique(NumberofCertifiedContracts$practice_name))),
    mainPanel(plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"))
        
    ))


