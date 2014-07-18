rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  data = read.csv('outcome-of-care-measures.csv',colClasses ='character')
  data[, 11] <- as.numeric(data[, 11])
  n = names(data)
  
  statelist = unique(data$State)
  dizlist = c("Heart.Attack","Heart.Failure","Pneumonia")
  t = unlist(strsplit(outcome," "))
  t1 = paste(t,collapse='.')
  t2 = grep(t1,dizlist,ignore.case=T,value=T)
  
  if ((state %in% statelist) & (length(t2)!=0)) {
    if (length(t)==1) {
      diz = t[1]      
    } else {
      diz = t[-1]
    }
    patern = paste0('^Hospital(.*)Mortality(.*)', diz,'$')
    
    colnum = grep(patern,n,ignore.case= T)
    data1 = data[data$State == state,]
    data2 = cbind(data1[,c('Provider.Number','Hospital.Name')],data1[,colnum])
    colnames(data2)[3] = "Rate"
    data3 = data2[(data2$Rate!="Not Available" & !is.na(data2$Rate)),]
    data3$Rate = as.numeric(as.character(data3$Rate))
    data4 = data3[order(data3$Rate,data3$Hospital.Name),]
    
    if (num=="best"){
      H = data4$Hospital.Name[1]
    } else if (num == "worst") {
      H = data4$Hospital.Name[nrow(data4)]
    } else {
      H = data4$Hospital.Name[as.numeric(num)]
    }
    
    return(H)
  } else if (!(state %in% statelist))  {
    stop("Error in best(", state, outcome, ") : invalid state")
  } else {
    stop("Error in best(", state, outcome, ") : invalid outcome")
  }
  
  
  
}