rankhospital <- function(state, outcome, num = "best") {
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
       ## by_state<-outcome_fulldata[outcome_fulldata$State==state,]
        subset_df<-outcome_fulldata[,c(2,7,outcomes[outcome])]

        ##changing column name of outcomes
        colnames(subset_df)[3]<- "Outcome"
        ##removing NAs
        clean_df<-na.omit(subset_df)
      

        ## Return hospital name in that state with the given rank 30-day death
        ## rate
        library(dplyr)
        ranking<-tbl_df(clean_df)
        
        by_state<-filter(ranking, State == state)
        ##print(by_state)
        rank_best<-arrange(by_state, Outcome, Hospital.Name)
        
        
        rank <- num
        if(rank == "best"){ 
                rank<- 1
        }else if(rank == "worst"){
                rank<- nrow(rank_best)
                }
        

        print(rank_best$Hospital.Name[rank])
        
        
}
        


