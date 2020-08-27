rankhospital<- function(state, outcome, num="best")
{
        outcome_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        valid_outcome<-c("heart attack","heart failure","pneumonia")
        
        #Validate the input state
        if(!state %in% outcome_data$State)
        {
                stop("invalid state")
        }
        
        #Validate the input outcome
        else if(!outcome %in% valid_outcome)
        {
                stop("invalid outcome")
        }
        
        #Call the helper function based on the input outcome
        else
        {
                if(outcome=="heart attack")
                {
                        hosp_name<- helper(outcome_data,11,state,num)        
                }
                else if(outcome=="heart failure")
                {
                        hosp_name<- helper(outcome_data,17,state,num)        
                }
                else if(outcome=="pneumonia")
                {
                        hosp_name<- helper(outcome_data,23,state,num)        
                }
        }
        return (hosp_name)
}

helper<- function(data,out_col,state,num)
{
        state_data<- data[data[,7]==state,]
        col_data<- state_data[,c(2,out_col)]
        col_data[,2]<- sapply(col_data[,2],as.numeric)
        comp_data<- na.omit(col_data)
        order_data<- comp_data[order(comp_data[,2],comp_data[,1]),]
        
        if (num=="worst")
        {
                num=nrow(order_data)
                hosp_name<- order_data[num,1]
                return (hosp_name)
        }
        else if(num=="best")
        {
                num=1
                hosp_name<- order_data[num,1]
                return (hosp_name)
        }
        else if(num>nrow(order_data))
        {
                return(NA)
        }
        else
        {
                hosp_name<- order_data[num,1]
                return (hosp_name)   
        }
}
