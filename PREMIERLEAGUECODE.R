library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)



load("PremierLeague.RData")

ui <- fluidPage(
  titlePanel("Premier League Team Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Bar Plot'",
        selectInput(inputId = "team1_bar",
                    label = "Choose first team:",
                    choices = team_name),
        uiOutput("barplot_team2_select"),
        uiOutput("barplot_team3_select"),
        selectInput(inputId = "barplot_location",
                    label = "Choose location:",
                    choices = c("Home", "Away")),
        checkboxGroupInput(inputId = "barplot_time",
                           label = "Choose time:",
                           choices = c("Half Time Result", "Full Time Result"),
                           selected = "Full Time Result")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Line Plot'",
        selectInput(inputId = "team1",
                    label = "Choose first team:",
                    choices = team_name),
        uiOutput("lineplot_team2_select"),
        uiOutput("lineplot_team3_select"),
        selectInput(inputId = "lineplot_location",
                    label = "Choose location:",
                    choices = c("Home", "Away"))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Violin Plot'",
        selectInput(inputId = "team1_violin",
                    label = "Choose first team:",
                    choices = team_name),
        uiOutput("violinplot_team2_select"),
        uiOutput("violinplot_team3_select"),
        selectInput(inputId = "violinplot_location",
                    label = "Choose location:",
                    choices = c("Home", "Away"))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("ReadMe",
                 br(),
                 p(strong("Welcome to the Premier League Team Analysis app! ")),
                 br(),
                 p("This interactive tool provides insights into the performance of various Premier League teams across different seasons. 
                   Explore key statistics, trends, and comparisons to gain a deeper understanding of how your favorite teams have fared."),
                 br(),
                 p("Features:"),
                 br(),
                 p("Bar Plot: Analyze match results for selected teams, locations, and time periods."),
                 p("Line Plot: Track cumulative points earned by teams over the course of a season."),
                 p("Violin Plot: Compare distributions of performance metrics such as corners based on team selection and match location."),
                 br(),
                 p("How to Use:"),
                 br(),
                 p("Select the desired analysis tab from the menu."),
                 p("Choose the teams, locations, and time periods you want to compare."),
                 p("Explore the generated plots to uncover insights into Premier League team performances."),
                 p("Start exploring now and discover fascinating insights into the world of Premier League football!")),
        tabPanel("Bar Plot", plotOutput("barplot")),
        tabPanel("Line Plot", plotOutput("lineplot")),
        tabPanel("Violin Plot", plotOutput("violinplot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$barplot_team2_select <- renderUI({
    team1bar_name <- input$team1_bar
    selectInput(inputId = "barplot_team2",
                label = "Choose second team:",
                choices = team_name[!team_name %in% team1bar_name],
                selected = "Not selected")
  })
  
  output$barplot_team3_select <- renderUI({
    team1bar_name <- input$team1_bar
    team2bar_name <- input$barplot_team2
    selectInput(inputId = "barplot_team3",
                label = "Choose third team:",
                choices = team_name[!team_name %in% c(team1bar_name, team2bar_name)],
                selected = "Unselected")
  })
  
  output$lineplot_team2_select <- renderUI({
    team1_name <- input$team1
    selectInput(inputId = "lineplot_team2",
                label = "Choose second team:",
                choices = team_name[!team_name %in% team1_name],
                selected = "Not selected")
  })
  
  output$lineplot_team3_select <- renderUI({
    team1_name <- input$team1
    team2_name <- input$lineplot_team2
    selectInput(inputId = "lineplot_team3",
                label = "Choose third team:",
                choices = team_name[!team_name %in% c(team1_name, team2_name)],
                selected = "Unselected")
  })
  
  output$violinplot_team2_select <- renderUI({
    team1violin_name <- input$team1_violin
    selectInput(inputId = "violinplot_team2",
                label = "Choose second team:",
                choices = team_name[!team_name %in% team1violin_name],
                selected = "Not selected")
  })
  
  output$violinplot_team3_select <- renderUI({
    team1violin_name <- input$team1_violin
    team2violin_name <- input$violinplot_team2
    selectInput(inputId = "violinplot_team3",
                label = "Choose third team:",
                choices = team_name[!team_name %in% c(team1violin_name, team2violin_name)],
                selected = "Unselected")
  })
  
  # Render the bar plot
  output$barplot <- renderPlot({
    team1bar_name <- input$team1_bar
    team2bar_name <- input$barplot_team2
    team3bar_name <- input$barplot_team3
    location <- input$barplot_location
    time <- ifelse("Half Time Result" %in% input$barplot_time, "HTR", "FTR")
    
    if (all(team1bar_name == "Please select", team2bar_name == "Not selected", team3bar_name == "Unselected")) {
      return(NULL)
    }
    else{
      # Filter data for selected teams and location
      team1bar_data <- soccer %>%
        filter((HomeTeam == team1bar_name & location == "Home") |
                 (AwayTeam == team1bar_name & location == "Away"))
      team2bar_data <- soccer %>%
        filter((HomeTeam == team2bar_name & location == "Home") |
                 (AwayTeam == team2bar_name & location == "Away"))
      team3bar_data <- soccer %>%
        filter((HomeTeam == team3bar_name & location == "Home") |
                 (AwayTeam == team3bar_name & location == "Away"))
      
      # Combine data for selected teams
      all_data <- bind_rows(
        mutate(team1bar_data, Team = team1bar_name),
        mutate(team2bar_data, Team = team2bar_name),
        mutate(team3bar_data, Team = team3bar_name)
      )
      
      # Count results by type for selected teams
      result_counts <- all_data %>% count(Team, !!sym(time)) %>%
        mutate(Result = ifelse(!!sym(time) == "H", "Win", 
                               ifelse(!!sym(time) == "A", "Lose", "Draw")))
      
    }
    
    
    # Plot data
    ggplot(result_counts, aes(x = Result, y = n, fill = Team)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Result",
           y = "Count",
           fill = " ",
           title = if (all(team1bar_name != "Please select", team2bar_name == "Not selected", team3bar_name == "Unselected")) {
             paste(location, time, "Results Breakdown -",input$team1_bar)
           } else if (all(team1bar_name != "Please select", team2bar_name != "Not selected", team3bar_name == "Unselected")) {
             paste(location, time, "Results Breakdown -",input$team1_bar, "vs", input$barplot_team2)
           } else {
             paste(location, time, "Results Breakdown -",input$team1_bar, "vs", input$barplot_team2, "vs", input$barplot_team3)
           }) + 
      theme_minimal() +
      theme(text = element_text(color = 'gray10'),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title.x = element_text(size=16, face="bold"),
            axis.text.x = element_text(size = 15),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank(),
            legend.text = element_text(size = 15),
            legend.title = ,
            legend.position = "bottom",
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size=16, face="bold"),
            strip.text = element_text(size = 14)) +
      scale_fill_manual(values = c("#ffc659", "#6cabdd", "#FF0000"))
  })
  
  # Render the line plot
  output$lineplot <- renderPlot({
    team1_name <- input$team1
    team2_name <- input$lineplot_team2
    team3_name <- input$lineplot_team3
    location <- input$lineplot_location 
    
    if (all(team1_name == "Please select", team2_name == "Not selected", team3_name == "Unselected")) 
    {return(NULL)}
    else{
      # Filter data for selected teams and location
      team1_data <- soccer %>%
        filter((HomeTeam == team1_name & location == "Home") |
                 (AwayTeam == team1_name & location == "Away"))
      team2_data <- soccer %>%
        filter((HomeTeam == team2_name & location == "Home") |
                 (AwayTeam == team2_name & location == "Away"))
      team3_data <- soccer %>%
        filter((HomeTeam == team3_name & location == "Home") |
                 (AwayTeam == team3_name & location == "Away"))
      
      # Combine data for selected teams
      all_data <- bind_rows(
        mutate(team1_data, Team = team1_name),
        mutate(team2_data, Team = team2_name),
        mutate(team3_data, Team = team3_name)
      )
      
      # Calculate cumulative points
      total_points <- all_data %>%
        mutate(points = ifelse((HomeTeam == Team & FTR == "H"), 3, 
                               ifelse((AwayTeam == Team & FTR == "A"), 3,
                                      ifelse((FTR == "D"), 1, 0))),
               CumulativePoints = ave(points, Team, FUN = cumsum),
               Date = as.Date(Date, "%d/%m/%Y"))
    }
    
    
    
    ggplot(total_points, aes(x = Date, y = CumulativePoints, color = Team)) +
      geom_line() +
      geom_point() +
      labs(title = if (all(team1_name != "Please select", team2_name == "Not selected", team3_name == "Unselected")) {
        paste(location, "Points Over Season", "-", input$team1)
      } else if (all(team1_name != "Please select", team2_name != "Not selected", team3_name == "Unselected")) {
        paste(location, "Points Over Season", "-", input$team1, "vs", input$lineplot_team2)
      } else {
        paste(location, "Points Over Season", "-", input$team1, "vs", input$lineplot_team2, "vs", input$lineplot_team3)
      },
      x = "Month",
      y = "Cumulative Points",
      color = " ") +
      theme_minimal() +
      theme(text = element_text(color = 'gray10'),
            plot.title = element_text(size = 18, face = "bold"),
            axis.title.x = element_text(size=16, face="bold"),
            axis.text.x = element_text(size = 15),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.y = element_blank(),
            legend.text = element_text(size = 15),
            legend.title = ,
            legend.position = "bottom",
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size=16, face="bold"),
            strip.text = element_text(size = 14)) +
      scale_color_manual(values = c("#ffc659", "#6cabdd","#FF0000"))
  })
  
  output$violinplot <- renderPlot({
    # Retrieve input values
    team1violin_name <- input$team1_violin
    team2violin_name <- input$violinplot_team2
    team3violin_name <- input$violinplot_team3
    location <- input$violinplot_location
    
    # Check if all necessary inputs are provided
    if (all(team1violin_name == "Please select", team2violin_name == "Not selected", team3violin_name == "Unselected")) {
      # Return NULL plot if no data is available
      return(NULL)
    } else {
      # Filter data for selected teams and location
      team1violin_data <- soccer %>%
        filter((HomeTeam == team1violin_name & location == "Home") |
                 (AwayTeam == team1violin_name & location == "Away"))
      
      team2violin_data <- soccer %>%
        filter((HomeTeam == team2violin_name & location == "Home") |
                 (AwayTeam == team2violin_name & location == "Away"))
      
      team3violin_data <- soccer %>%
        filter((HomeTeam == team3violin_name & location == "Home") |
                 (AwayTeam == team3violin_name & location == "Away"))
      
      all_data <- bind_rows(
        mutate(team1violin_data, Team = team1violin_name),
        mutate(team2violin_data, Team = team2violin_name),
        mutate(team3violin_data, Team = team3violin_name)
      )
    }
    
    # Plot the violin plot based on location
    if (location == "Home") {
      ggplot(all_data, aes(x = Team, y = HC, fill = Team)) +
        geom_violin(color = "black") +
        geom_boxplot(width = 0.1, color = "black", fill = "white") +
        coord_flip() +
        theme_classic() +
        labs(x = "Team",
             y = "Corners",
             fill = " ",
             title = if (all(team1violin_name != "Please select", team2violin_name == "Not selected", team3violin_name == "Unselected")) {
               paste("Distribution of Corners", location, "-", team1violin_name)
             } else if (all(team1violin_name != "Please select", team2violin_name != "Not selected", team3violin_name == "Unselected")) {
               paste("Distribution of Corners", location, "-", team1violin_name, "vs", team2violin_name)
             } else {
               paste("Distribution of Corners", location, "-", team1violin_name, "vs", team2violin_name, "vs", team3violin_name)
             }) +
        theme(text = element_text(color = 'gray10'),
              plot.title = element_text(hjust = 0.5, size=18, face="bold"),
              axis.title.x = element_text(size=16, face="bold"),
              axis.text.x = element_text(size = 15),
              panel.grid.major.x = element_blank(), 
              panel.grid.minor.y = element_blank(),
              legend.text = element_text(size = 15),
              legend.title = ,
              legend.position = "bottom",
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size=16, face="bold"),
              strip.text = element_text(size = 14)) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9", "#FF0000"))
    } else {
      ggplot(all_data, aes(x = Team, y = AC, fill = Team)) +
        geom_violin(color = "black") +
        geom_boxplot(width = 0.1, color = "black", fill = "white") +
        coord_flip() +
        theme_classic() +
        labs(x = "Team",
             y = "Corners",
             fill = "Team",
             title = if (all(team1violin_name != "Please select", team2violin_name == "Not selected", team3violin_name == "Unselected")) {
               paste("Distribution of Corners", location, "-", team1violin_name)
             } else if (all(team1violin_name != "Please select", team2violin_name != "Not selected", team3violin_name == "Unselected")) {
               paste("Distribution of Corners", location, "-", team1violin_name, "vs", team2violin_name)
             } else {
               paste("Distribution of Corners", location, "-", team1violin_name, "vs", team2violin_name, "vs", team3violin_name)
             }) +
        theme(text = element_text(color = 'gray10'),
              plot.title = element_text(hjust = 0.5, size=18, face="bold"),
              axis.title.x = element_text(size=16, face="bold"),
              axis.text.x = element_text(size = 15),
              panel.grid.major.x = element_blank(), 
              panel.grid.minor.y = element_blank(),
              legend.text = element_text(size = 15),
              legend.title = ,
              legend.position = "bottom",
              axis.text.y = element_text(size = 15),
              axis.title.y = element_text(size=16, face="bold"),
              strip.text = element_text(size = 14)) +
        scale_fill_manual(values = c("#E69F00", "#56B4E9", "#FF0000"))
    }
  })
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)