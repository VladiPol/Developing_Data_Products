library(shiny)
library(ggplot2)

observation_file_name <- read.csv("./data/V_NHL_15_16_DATA_MINING.csv", fileEncoding="UTF-8")

shinyUI(
    
    navbarPage("NHL (2015-2016): Goals per Game without Overtime",

    tabPanel("Explore the Data" ,

      sidebarPanel(
  
        selectInput('HOMETEAM', 'Home Team', sort(as.character(observation_file_name$HOMETEAM))),
        selectInput('VISITORTEAM', 'Visitor Team', sort(as.character(observation_file_name$VISITORTEAM))),
        sliderInput('SUM_TOTALSCORE_REGULAR_TIME', 'Sum of Goals per Game without Overtime', min=0, max=10, value=5.5, step=0.5, round=FALSE),
        submitButton("Submit")
      
      ),
  
      mainPanel(
        plotOutput("hist_GpG"),
        h4(textOutput("textOutput_Header")),
        textOutput("textOutput_Teams"),
        br(),
        textOutput("textOutput_YesNo"),
        textOutput("textOutput_Result")
      )
    
    ), # end of "Explore Dataset" tab panel
      
    tabPanel("About",
             mainPanel(
               includeMarkdown("about.md")
             )
    ) # end of "About" tab panel
    
  )
)