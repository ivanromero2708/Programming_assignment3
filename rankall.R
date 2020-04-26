rankall <- function(outcome, num = "best") {
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
      
      #Extracting State information
      outcome.data <- split(outcome.data,outcome.data$State)
      
      #Eliminating NA values in 
      outcome.data <- lapply(outcome.data, function(x) {
            if (outcome == "heart attack") {
                  x <- x[,c(2,7,11)]
                  library(dplyr)
                  x[,3] <- na_if(x[,3],"Not Available")
                  x <- na.omit(x)
            } else if (outcome == "heart failure"){
                  x <- x[,c(2,7,17)]
                  library(dplyr)
                  x[,3] <- na_if(x[,3],"Not Available")
                  x <- na.omit(x)
            } else if (outcome == "pneumonia"){
                  x <- x[,c(2,7,23)]
                  library(dplyr)
                  x[,3] <- na_if(x[,3],"Not Available")
                  x <- na.omit(x)
            }
            x[,3] <- as.character(x[,3])
            x[,3] <- as.numeric(as.character(x[,3]))
            hospital.rank <- x[order(x[,3],x[,1]),]
            if (num == "best"){
                  x <- hospital.rank[1,1]
            } else if (num == "worst") {
                  x <- hospital.rank[nrow(hospital.rank),1]
            } else if (num > nrow(hospital.rank)) {
                  x <- NA
            } else {
                  x <- hospital.rank[num,1]
            }
            x <- as.character(x)
      })
      outcome.data
}