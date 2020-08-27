best<- function(state, outcome)
{
        #Read the csv
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
                        hosp_name<- helper(outcome_data,11,state)        
                }
                else if(outcome=="heart failure")
                {
                        hosp_name<- helper(outcome_data,17,state)        
                }
                else if(outcome=="pneumonia")
                {
                        hosp_name<- helper(outcome_data,23,state)        
                }
        }
        return (hosp_name)
}

#Calculates the hospital name with min mortality rate
helper<- function(odata,out_col,state)
{
        #Get the desired data according to inputs and find the minimum mortality rate
        state_data<- odata[odata[,7]==state,]
        col_data<- as.numeric(state_data[,out_col])
        min_value<- min(col_data,na.rm = TRUE)
        
        #Get the index or indexes of the min value
        min_index<- which(col_data==min_value)

        #Get the hospital name of that index
        index_vector<-c()
        for (i in min_index)
        {
                y<-state_data[i,2]
                index_vector<-c(index_vector,y)
        }
        
        #In case of multiple hospitals, sort the vector and get the 1st value
        index_vector<- sort(index_vector)
        hosp_name<- index_vector[1]
        return (hosp_name)
}

