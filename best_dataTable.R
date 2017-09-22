best <- function(state, outcome) {
        ## Read outcome data
        outcome_fulldata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
         ## Check that state and outcome are valid
        if(any(outcome_fulldata$State == state)){
                ##state given is valid
                }
                else{
                        stop("invalid state")
                }
        if(outcome == "heart attack" | outcome =="heart failure" | outcome =="pneumonia"){
                ##outcome given is valid
                }
                else{
                        stop("invalid outcome")
                }
        
        ##subsetting data to outcome defined by user
        outcomes<-c("heart attack"=11, "heart failure"=17, "pneumonia" = 23)
        by_state<-outcome_fulldata[outcome_fulldata$State==state,]
        subset_df<-by_state[,c(2,7,outcomes[outcome])]

        ##changing column names to match outcomes
        colnames(subset_df)[3]<-outcome
        ##removing NAs
        clean_df<-na.omit(subset_df)
      

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        library(data.table)
        DT <- data.table(clean_df)
        
        min_outcome<- DT[State == state,.SD[which.min(clean_df[,3])],by=State]
        print(min_outcome$Hospital.Name)
}
        
        library(dplyr)
        ranking<-tbl_df(subset_df)
        
        result<-ranking%<%
                group_by(State)%<%
                select(Hospital.Name,
                        State, 
                        %<%
                omit.na()
                
        print(result)
                
                
       
        
        
        
        
        
        
