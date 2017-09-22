best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        subset_df<-outcome_data[,c(2,7,11,17,23)]
        ##changing column names
        colnames(subset_df)[3]<-"Heart.Attack"
        colnames(subset_df)[4]<-"Heart.Failure"
        colnames(subset_df)[5]<-"Pneumonia"
        

        
         ## Check that state and outcome are valid
        if(any(subset_df$State == state)){
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        library(data.table)
        DT <- data.table(subset_df)
        
        if(outcome == "heart attack"){
                min_HeartAttack<- DT[State == state,.SD[which.min(Heart.Attack)],by=State]
                print(min_HeartAttack$Hospital.Name)
                }else if(outcome == "heart failure"){
                        min_HeartFail<- DT[State == state,.SD[which.min(Heart.Failure)],by=State]
                        print(min_HeartFail$Hospital.Name)
                        }else{
                                min_Pnuemonia<- DT[State == state,.SD[which.min(Pneumonia)],by=State]
                                print(min_Pnuemonia$Hospital.Name)
                                }     

}
                
        



        
        
        
        library(dplyr)
        outcome2<-tbl_df(outcome)
        
        result<-outcome2%<%
                group_by(State)%<%
                select(Hospital.Name,
                        State, 
                        "Heart Attack" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                        "Heart Failure" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                        "Pneumonia" = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))%<%
                omit.na()
                
        print(result)
                
                
       
        
        
        
        
        
        
