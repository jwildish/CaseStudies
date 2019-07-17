#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

getwd()
Azle_Data <- read_excel("./Azle/Azle Data.xlsx", 
                        sheet = "Data")

Namelookup <- read_excel("./Azle/Azle Data.xlsx")
names(Namelookup)
Namelookup <- rename(Namelookup, "practice_code" = "100")
Namelookup <- rename(Namelookup, "practice_name" = "Comprehensive Nutrient Mgt Plan")
PracticeDescription <- read_excel("./Azle/Azle Data.xlsx", sheet = "Sheet3")





Azle_Data <- merge(Azle_Data, Namelookup, by= "practice_code")
Azle_Data <- merge(Azle_Data, PracticeDescription, by= "practice_code")

NumberofCertifiedContracts <- Azle_Data %>% group_by(practice_code, practice_name, CEAP_ESV_Landuse, definition, year) %>%
    summarise(payment = sum(payment, na.rm = TRUE),
              total.count = n(), 
              acresaffected = sum(land_unit_acres)) 

shinyServer(function(input, output, session) {
    updateRadioButtons(session, 'Type',
                             choices = unique(NumberofCertifiedContracts$practice_name))
    
    output$plot1 <- renderPlot({
        data <- NumberofCertifiedContracts
            data <- data[data$practice_name %in% input$Type,] 
            ggplot(data, aes(x=year, y=total.count, fill=CEAP_ESV_Landuse)) +
                geom_bar(stat="identity") +coord_flip() +theme_light() + scale_fill_manual(values=c("#112A12","#A3B43A", "#2252A4")) +
                ylab("Year") + xlab("") + ggtitle("Number of Certified Contracts")+ scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
            

        })
    output$plot2 <- renderPlot({
        data <- NumberofCertifiedContracts
        data <- data[data$practice_name %in% input$Type,] 
        ggplot(data ,aes(x=year, y=payment, fill=CEAP_ESV_Landuse)) +
            geom_bar(stat="identity") +coord_flip() +theme_light() + scale_fill_manual(values=c("#112A12","#A3B43A", "#2252A4")) +
            ylab("Year") + xlab("") + ggtitle("NRCS Payment to Producers") +scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
        
        
        
    })
    output$plot3 <- renderPlot({
        data <- NumberofCertifiedContracts
        data <- data[data$practice_name %in% input$Type,] 
        ggplot(data,aes(x=year, y=acresaffected, fill=CEAP_ESV_Landuse)) +
            geom_bar(stat="identity") +coord_flip() +theme_light() + scale_fill_manual(values=c("#112A12","#A3B43A", "#2252A4")) +
            ylab("Year") + xlab("") + ggtitle("Acres Affected by Certified Practices") +scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
        
        
    })
})

    


