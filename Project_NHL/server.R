library(shiny)
library(ggplot2)

score_data <- NULL

#
# The function prepares the data set for prediction model
#
get_teams <- function(hometeam,
                      visitorteam,
                      score_data)
{
  # return value
  output_data_frame <- data.frame()
  
  # get data from whole score data set
  hometeam_subset <- subset(score_data, score_data$HOMETEAM == hometeam)
  # the maximal ROWNUM with home team name gives the goals information
  hometeam_data <- subset(hometeam_subset, hometeam_subset$ROWNUM == max(hometeam_subset$ROWNUM)) 
  # get home team goals and against goals from the previous game
  hometeam_prev_goals <- hometeam_data$HOME_TOTALSCORE
  hometeam_prev_against_goals <- hometeam_data$VISITOR_TOTALSCORE
  # get the sum home team goals and against goals from the previous game 
  hometeam_goals <- hometeam_data$HOME_SUM_PREV_GOALS + hometeam_prev_goals
  hometeam_against_goal <- hometeam_data$HOME_SUM_PREV_AGAINST_GOALS + hometeam_prev_against_goals    
  
  # get data from the whole score dataset
  visitorteam_subset <- subset(score_data, score_data$VISITORTEAM == visitorteam)
  # the maximal ROWNUM with home team name gives the goals information
  visitorteam_data <- subset(visitorteam_subset, visitorteam_subset$ROWNUM == max(visitorteam_subset$ROWNUM))   
  # get visitor team goals and against goals from the previous game
  visitorteam_prev_goals <- visitorteam_data$VISITOR_TOTALSCORE
  visitorteam_prev_against_goals <- visitorteam_data$HOME_TOTALSCORE
  # get the sum visitor team goals and against goals from the previous game
  visitorteam_goals <- visitorteam_data$VISITOR_SUM_PREV_GOALS + visitorteam_prev_goals
  visitorteam_against_goal <- visitorteam_data$VISITOR_SUM_PREV_AGAINST_GOALS + visitorteam_prev_against_goals
  
  # get goal difference for the output dataset
  diff_prev_goals <- hometeam_goals - visitorteam_goals
  diff_prev_against_goals <- hometeam_against_goal - visitorteam_against_goal
  
  # cat("HOMETEAM: ", hometeam, " VISITORTEAM: ", visitor, "\n")
  # print(c(hometeam,visitor,hometeam_goals,hometeam_against_goal,visitor_goals,visitor_against_goal,diff_prev_goals,diff_prev_against_goals))
  output_data_frame <- rbind(output_data_frame,data.frame(hometeam,visitorteam,hometeam_prev_goals,hometeam_prev_against_goals,hometeam_goals,hometeam_against_goal,visitorteam_prev_goals,visitorteam_prev_against_goals,visitorteam_goals,visitorteam_against_goal,diff_prev_goals,diff_prev_against_goals))  


  names(output_data_frame) <- c("HOMETEAM","VISITORTEAM","HOME_PREV_GOALS","HOME_PREV_AGAINST_GOALS","HOME_SUM_PREV_GOALS","HOME_SUM_PREV_AGAINST_GOALS","VISITOR_PREV_GOALS","VISITOR_PREV_AGAINST_GOALS","VISITOR_SUM_PREV_GOALS","VISITOR_SUM_PREV_AGAINST_GOALS","DIFF_PREV_GOALS","DIFF_PREV_AGAINST_GOALS")
  # print(output_data_frame)
  return(output_data_frame)
}

#
# The function predicts the under/over (dynamic) game score without the overtime 
# dependent on score difference before the game
# The input parameters are:
#
# 1. Hometeam                        - Name of Hometeam
# 2. Visitorteam                     - Name of Visitorteam
# 3. The under/over limit            - default = 5 Goals per Game
# 4. sample_size                     - Sample size of data for model between 0 and 1 (default = 0.7 --> 70% of observation data)
#                                      NOT YET dynamic as parameter from ui.R
#
predict_eishockey_under_over_dynamic <- function(hometeam,
                                                 visitorteam,
                                                 under_over_limit=5,
                                                 sample_size=0.7)
{
  # get observation dataset (NHL Games for season 2015 / 2016)
  if (is.null(score_data)){
    score_data <- read.csv("./data/V_NHL_15_16_DATA_MINING.csv", fileEncoding="UTF-8")
  }
  # print(score_data)
  
  # get dataset with teams for prediction
  input_current_frame <- get_teams(hometeam, visitorteam, score_data)
  # print("The input data are:")
  # print(input_current_frame)
  
  ##########################################
  # Init the input variables
  ##########################################
  # Set seed
  model_seed <- 12
  
  ################################################
  # 1. Predict the under/over goals 
  #    per game with Decision Tree and 'rpart' 
  #    package
  ################################################
  require(rpart)
  
  
  # Reset the random number seed to obtain the same results each time
  set.seed(model_seed)
  
  # The sample size of observation is the whole input dataset
  # If it is necessary please change the sample size below (default 70% of the observation data)
  # ####################################################################
  number_observation <- nrow(score_data)
  score_data_sample <- sample(nrow(score_data), sample_size*number_observation)
  # print(score_data_sample)
  # ####################################################################
  
  # get data for under_over_limit as condition SUM_TOTALSCORE <= under_over_limit (Yes/No)
  # and add column to the main data frame
  score_data$input_target_less_dynamic <- ifelse(score_data$SUM_TOTALSCORE_REGULAR_TIME <= under_over_limit, "Yes", "No")
  
  
  # target variable (less then under_over_limit baskets per game Yes/No)
  input_target_less_dynamic  <- paste("SUM_TTL_RGLR_TIME_LESS_", under_over_limit, sep="")
  # rename the "input_target_less_dynamic" to the SUM_TTL_RGLR_TIME_LESS_<LIMIT>
  names(score_data)[names(score_data) == "input_target_less_dynamic"] <- input_target_less_dynamic
  # str(score_data)
  
  # input variables or predictor variables 
  input_predictors <- c("HOME_PREV_GOALS","HOME_PREV_AGAINST_GOALS","VISITOR_PREV_GOALS","VISITOR_PREV_AGAINST_GOALS","DIFF_PREV_GOALS","DIFF_PREV_AGAINST_GOALS")
  
  # ###################################################################
  # ####### The sample size is 70% DEFAULT  (s. comments above) #######
  # ###################################################################
  # Build the Decision Tree model for SUM_TTL_RGLR_TIME_LESS_<LIMIT> as "classification" tree (less then limit baskets per game Yes/No)    
  sum_ttl_less_dynamic_model <- rpart( as.formula(paste(input_target_less_dynamic, " ~ .", sep="")),
                                       data=score_data[score_data_sample, c(input_predictors, input_target_less_dynamic)],
                                       method="class",
                                       parms=list(split="information"),
                                       control=rpart.control(usesurrogate=0, 
                                                             maxsurrogate=0))
  
  # Generate a textual view of the Decision Tree model  
  # print(sum_ttl_less_dynamic_model)
  # printcp(sum_ttl_less_dynamic_model)  
  # cat("\n")
  
  # Predict the sum of total score for the input data
  sum_ttl_less_dynamic_predict <- predict(sum_ttl_less_dynamic_model, newdata=input_current_frame)  
  
  # print(sum_ttl_less_dynamic_predict)  
  
  # Print the prediction by Decision Tree model  
  # cat("The prediction is:", sum_total_score_predict, "\n")  
  # Put the prediction to the input dataset as the last column
  input_current_frame <- cbind(input_current_frame, 
                               sum_ttl_less_dynamic_predict)  
  # print(input_current_frame)
  
  # prepare output
  # print("The prediction is:")  
  output_prediction <- cbind(input_current_frame[,c(1,2)], 
                             signif(sum_ttl_less_dynamic_predict,2))
  output_limit_less_no <- paste("LESS_", under_over_limit, "_NO", sep="")
  output_limit_less_yes <- paste("LESS_", under_over_limit, "_YES", sep="")
  names(output_prediction) <- c("HOMETEAM","VISITORTEAM",
                                output_limit_less_no,output_limit_less_yes)
  # print(names(output_prediction))
  # print(output_prediction)
  return(output_prediction)
}

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