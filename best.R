best <- function(state, outcome) {
  is_in <- function(x,data){
    data<-data[!is.na(data)]
    for(i in 1:length(data)){
      
      if(x==data[i]){return(TRUE)}
    }
    return(FALSE)
  }
  
  ## Read outcome data
  oc <- c("heart attack","heart failure","pneumonia")
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!is_in(state,data$State)){stop("invalid state")}
  if(!is_in(outcome,oc)){stop("invalid outcome")}
  ## Return hospital name in that state with lowest 30-day death

  s=split(data,as.factor(data$State),drop = T)
  s=s[[state]]
  head(s$State)
  if(outcome=="heart attack"){datarates<-s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack}
  else if(outcome=="heart failure"){datarates<-s$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure}
  else{datarates<-s$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia}
  datarates<-as.numeric(datarates)
  mini<-datarates[!is.na(datarates)]
  mini<-min(mini)
  count=0
  name<-c()

  for(i in 1:length(datarates)){
    if(is.na(datarates[i])){next}
    if(datarates[i]==mini){
      count=count+1
      name[count]<-s$Hospital.Name[i]
      #"print(count)
      #print(name)
    }
    

  }
  
  rates<-sort(name)
  #print(count)
  ## rate
  return(rates[1])
}
best("TX", "heart failure")
best("TX","heart attack")
best("TX","pneumonia")
