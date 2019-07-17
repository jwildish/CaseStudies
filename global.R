
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

Azle_Data <- read_excel("https://www.dropbox.com/s/fzqhgrjxgmrc5yo/Azle%20Data.xlsx?dl=0", 
                        sheet = "Data")

Namelookup <- read_excel("./Azle Data.xlsx")
names(Namelookup)
Namelookup <- rename(Namelookup, "practice_code" = "100")
Namelookup <- rename(Namelookup, "practice_name" = "Comprehensive Nutrient Mgt Plan")
PracticeDescription <- read_excel("./Azle Data.xlsx", sheet = "Sheet3")



Azle_Data <- merge(Azle_Data, Namelookup, by= "practice_code")
Azle_Data <- merge(Azle_Data, PracticeDescription, by= "practice_code")

NumberofCertifiedContracts <- Azle_Data %>% group_by(practice_code, practice_name, CEAP_ESV_Landuse, definition, year) %>%
  summarise(payment = sum(payment, na.rm = TRUE),
            total.count = n(), 
            acresaffected = sum(land_unit_acres)) 
