library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidytuesdayR)

        
        
tuesdata <- tidytuesdayR::tt_load('2023-04-04')
      
soccer <- tuesdata$soccer


team_name <- c("Please select", "Not selected", "Unselected", "Arsenal", "Aston Villa", "Brentford", "Brighton", "Burnley", 
               "Chelsea", "Crystal Palace", "Everton", "Leeds", "Leicester", 
               "Liverpool", "Man United", "Man City", "Newcastle", "Norwich", 
               "Southampton", "Tottenham", "Watford", "West Ham", "Wolves")



save(soccer, team_name,
     file="PremierLeague.RData")