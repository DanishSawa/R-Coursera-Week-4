library(dplyr)

rankall<- function(outcome, num="best")
{
        #Read the csv
        outcome_data<- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        valid_outcome<-c("heart attack","heart failure","pneumonia")
 
        #Validate the input outcome
        if(!outcome %in% valid_outcome)
        {
                stop("invalid outcome")
        }
        
        #Call the helper function based on the input outcome
        else
        {
                if(outcome=="heart attack")
                {
                        hosp_rank<- helper(outcome_data,11,num)        
                }
                else if(outcome=="heart failure")
                {
                        hosp_rank<- helper(outcome_data,17,num)        
                }
                else if(outcome=="pneumonia")
                {
                        hosp_rank<- helper(outcome_data,23,num)        
                }
        }
        return(hosp_rank)
}

helper<- function(data, out_col, num)
{
        #Get the desired data according to inputs
        req_data<- data[,c(2,7,out_col)]
        req_data[,3]<- sapply(req_data[,3],as.numeric)
        comp_data<- na.omit(req_data)
        order_data<- comp_data %>% arrange(comp_data[,2],comp_data[,3],comp_data[,1])
        filter_data<- order_data[,c(1,2)]
        split_data<- split(filter_data, filter_data$State)
        
        l<- length(split_data)
        df<- data.frame()
        
        #Based on num value, extract the result from filtered data
        if(num=="best")
        {
                num<-1
        }
        
        for (i in 1:l)
        {
                #state_name<- split_data[[i]][1,2]
                #hosp_name<- split_data[[i]][num,1]
                #df<- rbind(df,c(hosp_name,state_name)) ;Error: invalid subscript type 'list' 
                        
                vector_data<- unlist(split_data[i])
                n<- length(vector_data)
                state_name<- vector_data[n]
                if(num=="worst")
                {
                        hosp_name<- vector_data[n/2]
                }
        
                else 
                {
                        hosp_name<- vector_data[num]
                }
        
                if (hosp_name==state_name || is.na(hosp_name))
                {
                        hosp_name<-NA
                }
                        
                df<- rbind(df,c(hosp_name,state_name))
                        
        }
        
        names(df)<- c("hospital","state")
        return(df)
}
        

