rankall <- function(outcome, num = "best") {
  ## Read outcome data
  setwd("C:\\Users\\Robert\\Coursera\\2. R Programming\\A3")
  outcomeList <- c("heart attack","heart failure","pneumonia")
  outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  library(datasets)
  data(state)  
  if (!outcome %in% outcomeList) {
    stop ("invalid outcome")
  }
  
  #2. Hospital Name: varchar (50) Lists the name of the hospital.
  #7. State: varchar (2) Lists the 2 letter State code in which the hospital is located. 
  #11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital. 
  #17. Hospital 30-Day Death(Mortality) Rates from Heart Failure: Lists the risk adjusted rate (percentage) for each hospital.
  #23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the risk adjusted rate (percentage) for each hospital. 
  outcomeCol = switch(outcome,"heart attack" = 11,"heart failure" = 17,"pneumonia" = 23)
  
  outcomeDF <- data.frame(outcomeData[,][,2],
                          outcomeData[,][,7],
                          suppressWarnings(as.numeric(outcomeData[,][,outcomeCol])),stringsAsFactors=FALSE)
  
  colnames(outcomeDF) <- c("name","state","outcome")
  
  # minimumOD <- min(outcomeDF$outcome,na.rm=TRUE)
  # outcomeDF <- outcomeDF[complete.cases(outcomeDF$outcome),]
  outcomeDF <- outcomeDF[with(outcomeDF,order(outcome,name)),]
  
  if (num == "best") rank = 1
  else if (num == "worst") rank = nrow(outcomeDF)
  else rank = num
  
  if (rank < 1 || rank > nrow(outcomeDF)){
    NA
  } else
  {
    #outcomeDF$name[rank]
    #head(outcomeDF,rank)
    #aggregate(outcomeDF,by=list(outcomeDF$state), function(g) g[rank])[,c("name","state")]
    os <- split(outcomeDF,outcomeDF$state)
    sapply(os,function (g) g[rank,1])
  }

}
