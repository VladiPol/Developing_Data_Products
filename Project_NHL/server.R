library(shiny)
library(ggplot2)

score_data <- NULL

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
        paste("YES", " NO")
        
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