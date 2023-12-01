library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)
library(htmlwidgets)
library(tidyverse)
library(knitr)
library(kableExtra)
library(shiny)
library(rsconnect)
rsconnect::setAccountInfo(name='growlfood',
                          token='210AA88C706CE7487AD1772F92776C36',
                          secret='5ghsDbCcVCXD3yTZVtKztgMpJssZ2IXvZxPcO2PG')
#rsconnect::deployApp('Desktop/SDS 313/Projects & Labs/Project 3')


#Copying datasets from Project 2 to create final dataset for use in Project 3
#Loading datasets
setwd("~/Desktop/SDS 313/Projects & Labs/Project 3")
connelly_sp_plus <- read.csv("Connelly SP+ File - SP+.csv")
connelly_w1 <- read.csv("Connelly SP+ File - FBS W1.csv")
connelly_w2 <- read.csv("Connelly SP+ File - FBS W2.csv")
connelly_w3 <- read.csv("Connelly SP+ File - FBS W3.csv")
connelly_w4 <- read.csv("Connelly SP+ File - FBS W4.csv")
connelly_w5 <- read.csv("Connelly SP+ File - FBS W5.csv")
connelly_w6 <- read.csv("Connelly SP+ File - FBS W6.csv")
connelly_w7 <- read.csv("Connelly SP+ File - FBS W7.csv")
connelly_w8 <- read.csv("Connelly SP+ File - FBS W8.csv")

#Merging weekly datasets into one season-long dataset
full_season <- rbind(connelly_w1, connelly_w2, connelly_w3, connelly_w4, connelly_w5, connelly_w6, connelly_w7, connelly_w8) 
full_season <- na.omit(full_season)

#Standardizing date variable
full_season$Date <- as.Date(full_season$Date, format = "%d-%b")
connelly_sp_plus$Date <- as.Date(connelly_sp_plus$Date, format = "%d-%b")

#Removing unnecessary clutter from full season dataset
full_season$Time..ET. = NULL
full_season$Proj..margin = NULL
full_season$Win.prob. = NULL
full_season$Proj..score..rounded. = NULL
full_season$ATS.Pick = NULL
full_season$Spread.diff = NULL
full_season$O.U = NULL
full_season$O.U.pick = NULL
full_season$O.U.diff = NULL
full_season <- full_season %>%
  rename(Favorite = Proj..winner)

#Parsing the spread
full_season$Spread <- sub(".*-", "", full_season$Spread)

#Extracting home and away teams
full_season$Home_Team <- sub(".* at", "", full_season$Game)
full_season$Away_Team <- str_extract(full_season$Game, ".*(?= at)")

#Getting rid of neutral site games
full_season <- full_season %>%
  filter(!grepl("vs\\.", Home_Team))

#Removing away and neutral-site games, so there's no double-counting games in SP+ dataset
connelly_sp_plus <- connelly_sp_plus %>% 
  filter(H.A != "A") %>%
  filter(H.A != "N")

#Removing unnecessary clutter from SP+ dataset
connelly_sp_plus$GameType = NULL
connelly_sp_plus$Score = NULL
connelly_sp_plus$OppScore = NULL
connelly_sp_plus$Adj.Mgn = NULL
connelly_sp_plus$Conf = NULL
connelly_sp_plus$Margin = NULL
connelly_sp_plus$H.A = NULL
connelly_sp_plus <- connelly_sp_plus %>%
  rename(Home_Team = Team)
connelly_sp_plus <- connelly_sp_plus %>%
  rename(Away_Team = Opponent)


#Realigning variable classes for easier manipulation
connelly_sp_plus$W.L <- as.numeric(connelly_sp_plus$W.L)
connelly_sp_plus$PGWE <- substring(connelly_sp_plus$PGWE, 1, nchar(connelly_sp_plus$PGWE) - 1)
connelly_sp_plus$PGWE <- parse_number(connelly_sp_plus$PGWE)

#Merging datasets to have date, home team, away team, PGWE, spread, etc
merged_data <- merge(connelly_sp_plus, full_season, by = c("Date", "Away_Team"))
merged_data$Game = NULL

#Reformatting merged dataset
merged_data <- merged_data %>%
  rename(Home_Team = Home_Team.x)
merged_data$Home_Team.y = NULL
merged_data$Spread <- as.numeric(merged_data$Spread)
merged_data <- merged_data %>%
  select("Date", "Home_Team", "Away_Team", "Favorite", "Spread", "W.L", "PGWE")


#Getting rid of rows with abbreviated team names, for sake of simplicity when trying to find who is favored
merged_data <- subset(merged_data, Favorite == Home_Team | Favorite == Away_Team)

#Determining whether home team won
merged_data$Winner <- ifelse(merged_data$W.L == 1, merged_data$Home_Team, merged_data$Away_Team)

#Determining who won the game
merged_data$Expected_Outcome <- ifelse(merged_data$Winner == merged_data$Favorite, TRUE, FALSE)
merged_data$W.L = NULL

#Creating a variable to determine how much PGWE the favorite
merged_data$PGWE <- merged_data$PGWE/100
merged_data$Favorite_PGWE <- ifelse(merged_data$Expected_Outcome == TRUE, merged_data$PGWE, (1 - merged_data$PGWE))
merged_data <- merged_data %>%
  select("Date", "Home_Team", "Away_Team", "Favorite", "Spread", "Favorite_PGWE", "PGWE", "Expected_Outcome")
merged_data$PGWE = NULL

merged_data <- na.omit(merged_data)








#Creating Shiny App
ui <- fluidPage(
  titlePanel("College Football Pregame Betting Spreads compared against Postgame Win Expectancies"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable", choices = colnames(merged_data), selected = "Spread"),
      sliderInput("range", "Select a range", min = min(merged_data$Spread), max = max(merged_data$Spread), value = c(min(merged_data$Spread), max(merged_data$Spread))),
      actionButton("updateBtn", "Update Graph"),
      selectInput("color", "Choose a color", choices = c("red", "blue", "green"), selected = "blue")
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("stats")
    )
  )
)

#Creating server and graphs
server <- function(input, output) {
  filtered_data <- reactive({
    merged_data %>%
      filter(Spread >= input$range[1] & Spread <= input$range[2])
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$variable)) +
      geom_bar(fill=input$color) +
      labs(title = paste("Distribution of", input$variable),
           x = input$variable,
           y = "Count") +
      theme_minimal()
  })
  
  output$stats <- renderPrint({
    summary(filtered_data()[, input$variable])
  })

  observeEvent(input$updateBtn, {
  })
}

shinyApp(ui, server)

runGitHub( "Lundblad_Project3", "growlfood")
