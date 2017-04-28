best<- function(state,outcome)
{
  ## Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  csv[,11]<- as.numeric(csv[,11]) #heart attack
  csv[,17]<- as.numeric(csv[,17]) #heart failure
  csv[,23]<- as.numeric((csv[,23])) #pnue
 
   ## Check that state and outcome are valid
  outcomeTypes <-c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in%  outcomeTypes)
  stop("not a valid outcome")
  if(!state %in%  csv$State)
  stop("not a valid sate")
  
  ## Return hospital name in that state with lowest 30-day death
  if(outcome == "heart attack")
  {
    filByState <- csv[csv[,7]==state,]## all rows filtered by state
    filterbyHA <- filByState[,11]
    minHA <- min(filterbyHA,na.rm = "T")
    indHosp <- which(filterbyHA==minHA)
    hosp<- filByState[indHosp,2]
  }
  else if (outcome == "heart failure")
  {
    filByState <- csv[csv[,7]==state,]## all rows filtered by state
    filterbyHA <- filByState[,17]
    minHA <- min(filterbyHA,na.rm = "T")
    indHosp <- which(filterbyHA==minHA)
    hosp<- filByState[indHosp,2]
    
  }
  else if(outcome == "pneumonia")
  {
    filByState <- csv[csv[,7]==state,]## all rows filtered by state
    filterbyHA <- filByState[,23]
    minHA <- min(filterbyHA,na.rm = "T")
    indHosp <- which(filterbyHA==minHA)
    hosp<- filByState[indHosp,2]
  }
  ## rate
hosp
}

