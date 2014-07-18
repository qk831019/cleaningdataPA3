rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data = read.csv('outcome-of-care-measures.csv',colClasses ='character')
  data[, 11] <- as.numeric(data[, 11])
  n = names(data)
  statelist = unique(data$State)
  dizlist = c("Heart.Attack","Heart.Failure","Pneumonia")
  t = unlist(strsplit(outcome," "))
  t1 = paste(t,collapse='.')
  t2 = grep(t1,dizlist,ignore.case=T,value=T)
  
  if (length(t2)!=0) {
    if (length(t)==1) {
      diz = t[1]      
    } else {
      diz = t[-1]
    }
    patern = paste0('^Hospital(.*)Mortality(.*)', diz,'$')
    
    colnum = grep(patern,n,ignore.case= T)
    data2 = cbind(data[,c('Provider.Number','State','Hospital.Name')],data[,colnum])
    colnames(data2)[4] = "Rate"
    data3 = data2[(data2$Rate!="Not Available" & !is.na(data2$Rate)),]
    data3$Rate = as.numeric(as.character(data3$Rate))
    data4 = data3[order(data3$State,data3$Rate,data3$Hospital.Name),]
    
    rkall = data.frame(statelist,row.names=statelist)
    names(rkall) = "state"
    l = split(data4,data4$State)
    for (st in statelist) {
      data5= l[[st]]
      if (num=="best"){
        H = data5$Hospital.Name[1]
      } else if (num == "worst") {
        H = data5$Hospital.Name[nrow(data5)]
      } else {
        H = data5$Hospital.Name[as.numeric(num)]
      }
      rkall[st,'hospital'] = H
    }

    return(rkall)
  } else {
    stop("Error in best(",outcome, ") : invalid outcome")
  }
  
  
  
}