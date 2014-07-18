best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data = read.csv('outcome-of-care-measures.csv',colClasses ='character')
  data[, 11] <- as.numeric(data[, 11])
  
  statelist = unique(data$State)
  dizlist = c("Heart.Attack","Heart.Failure","Pneumonia")
  t = unlist(strsplit(outcome," "))
  t1 = paste(t,collapse='.')
  t2 = grep(t1,dizlist,ignore.case=T,value=T)
  
  if (state %in% statelist) & (length(t2)!=0) { 
    if (length(t)==1) {
      diz = t[1]       
    } else {diz = t[-1]}
    patern = paste0('^Hospital(.*)Mortality(.*)', diz,'$')
    
    colnum = grep(patern,n,ignore.case= T)
    data1 = data[data$State == state,]
    return(data1[which.min(data1[,colnum]),2])
  }
  else if (!(state %in% statelist))  {
    message("Error in best(", state, outcome, ") : invalid state")
  }
  else {
    message("Error in best(", state, outcome, ") : invalid outcome")
  }
}