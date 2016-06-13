library(shiny)
library(ggplot2)

observation_file_name <- read.csv("./data/V_NHL_15_16_DATA_MINING.csv", fileEncoding="UTF-8")

shinyUI(

  fluidPage(

    titlePanel("NHL (2015-2016): Goals per Game without Overtime"),

    sidebarPanel(

      selectInput('HOMETEAM', 'Home Team', sort(as.character(observation_file_name$HOMETEAM))),
      selectInput('VISITORTEAM', 'Visitor Team', sort(as.character(observation_file_name$VISITORTEAM))),
      sliderInput('SUM_TOTALSCORE_REGULAR_TIME', 'Sum of Goals per Game without Overtime', min=0, max=10, value=5.5, step=0.5, round=FALSE),
      submitButton("Submit")
    
    ),

    mainPanel(
      textOutput("textOutput_Header"),
      textOutput("textOutput_Teams"),
      textOutput("textOutput_YesNo"),
      textOutput("textOutput_Result")
    )
  )
)