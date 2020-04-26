best <- function(state,outcome) {
      ## Read outcome data
      # To achieve this the file "outcome-of-care-measures.csv" must be unzipped in the Project's working directory
      outcome.data <- read.csv(paste(getwd(), "outcome-of-care-measures.csv", sep = "/"))
      
      ## Check that state and outcome are valid
      # Checking outcome
      if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
           stop_msg1 <- paste0("Error in best('", state,"','", outcome,"') : invalid outcome")
           stop(stop_msg1)
      }
      
      #Extracting states
      States <- unique(outcome.data$State)
      
      #Checking states
      confirmed.state <- FALSE
      for (i in seq_along(States)){
            if (States[i] == state) {
                 confirmed.state <- TRUE 
            }
      }
      if (confirmed.state == FALSE) {
            stop_msg2 <- paste0("Error in best('", state,"','", outcome,"') : invalid state")
            stop(stop_msg2)
      }
      
      ##Extracting State information
      outcome.data <- split(outcome.data,outcome.data$State)
      outcome.data <- outcome.data[[state]]
      
      ##Selecting the best hospital
      if (outcome == "heart attack") {
            outcome.data <- outcome.data[,c(2,7,11)]
            library(dplyr)
            outcome.data[,3] <- na_if(outcome.data[,3],"Not Available")
            outcome.data <- na.omit(outcome.data)
      } else if (outcome == "heart failure"){
            outcome.data <- outcome.data[,c(2,7,17)]
            library(dplyr)
            outcome.data[,3] <- na_if(outcome.data[,3],"Not Available")
            outcome.data <- na.omit(outcome.data)
      } else if (outcome == "pneumonia"){
            outcome.data <- outcome.data[,c(2,7,23)]
            library(dplyr)
            outcome.data[,3] <- na_if(outcome.data[,3],"Not Available")
            outcome.data <- na.omit(outcome.data)
      }
      outcome.data[,3] <- as.character(outcome.data[,3])
      outcome.data[,3] <- as.numeric(as.character(outcome.data[,3]))
      outcome.data <- outcome.data[order(outcome.data[,3],outcome.data[,1]),]
      best.hospital <- outcome.data[1,1]
      as.character(best.hospital)
}