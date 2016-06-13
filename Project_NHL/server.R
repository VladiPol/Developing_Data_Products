library(shiny)
library(ggplot2)

# get observation dataset (NHL Games for season 2015 / 2016)
score_data <- read.csv("./data/V_NHL_15_16_DATA_MINING.csv", fileEncoding="UTF-8")

# Load model (rpart) functions
source("rpart_model.R", local = TRUE)

doPrediction <- function(hometeam,
                         visitorteam)
{
  if ( hometeam != "" && visitorteam != ""){
    if( hometeam == visitorteam ){
      # warning("The Hometeam could not be the same as the Visitorteam")
      return(FALSE)
    }
    else{
      return(TRUE)
    }
    return(FALSE)
  }
  else{
    return(FALSE)
  }
}

shinyServer(

  function(input, output) {
    
    # Output histogram
    output$hist_GpG <- renderPlot({
      h  <- hist(score_data$SUM_TOTALSCORE_REGULAR_TIME, xlab = "Sum Total Goals per Game without Overtime", main = "Goals per Game in the Regular Time (NHL 2015 / 2016)")
        
      print(h)
    })
    
    output$textOutput_Header <- renderText({
      if ( doPrediction(input$HOMETEAM, input$VISITORTEAM) ){
        
        # Output title
        paste("Prediction of Goals per Game without Overtime for:")
        
      }
      else{
        
        # Output title
        paste("Choose two different Teams for Prediction of Goals per Game without Overtime.")
        
      }
    })
    
    output$textOutput_Teams <- renderText({
      if ( doPrediction(input$HOMETEAM, input$VISITORTEAM) ){
        
        # print the obective of prediction
        paste(input$HOMETEAM, " vs. ", input$VISITORTEAM, " less than ", input$SUM_TOTALSCORE_REGULAR_TIME, " Goals per Game without Overtime")
        
      }  
    })
    
    output$textOutput_YesNo <- renderText({
      if ( doPrediction(input$HOMETEAM, input$VISITORTEAM)){
        
        # print Yes / No
        paste("YES", "NO")
        
      }  
    })
    
    output$textOutput_Result <- renderText({
      if ( doPrediction(input$HOMETEAM, input$VISITORTEAM) ){
        
        # get prediction
        output_prediction <- predict_eishockey_under_over_dynamic(input$HOMETEAM, input$VISITORTEAM, input$SUM_TOTALSCORE_REGULAR_TIME)
        
        # print the result of Prediction (%)
        paste(output_prediction[4], "%", output_prediction[3], "%")
      }  
    })

  }
  
)