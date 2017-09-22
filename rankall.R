rankall <- function(outcome, num = "best"){
        ## Read outcome data
        outcome_fulldata <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
         ## Check that outcome is valid
       
        if(outcome == "heart attack" | outcome =="heart failure" | outcome =="pneumonia"){
                ##outcome given is valid
                }
                else{
                        stop("invalid outcome")
                }
        
        ##subsetting data to outcome defined by user
        outcomes<-c("heart attack"=11, "heart failure"=17, "pneumonia" = 23)
      
        subset_df<-outcome_fulldata[,c(2,7,outcomes[outcome])]

        ##changing column name of outcomes
        colnames(subset_df)[3]<- "Outcome"
        ##removing NAs
        clean_df<-na.omit(subset_df)
      
        library(dplyr)
        clean_tbl_df<-tbl_df(clean_df)
        ## For each state, find the hospital of the given rank
        
        by_state<-group_by(clean_tbl_df, State)
        rank_num<-arrange(by_state, Outcome, Hospital.Name)
        by_rank<-mutate(rank_num, ranking=row_number(State))
        ##View(by_rank)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        
        
        #count<-summarize(by_state, n())
        #View(count)

        rank <- num
        if(rank == "best"){ 
                result<-slice(by_rank, first(ranking))
               
                }else if(rank =="worst" ){
                        result<-slice(by_rank, last(ranking))
                        }else{result<-filter(by_rank, ranking %in% rank)
                                }
        
        ##getting hospitals of equivalant rank
        
        
        all_states<-summarise(by_rank, count = n())
        summary_result<-full_join(all_states, result)
        ##View(summary_result)
        
        final<-select(summary_result, Hospital.Name, State)
        
        #print hospital names and state
        print(final)
}
              
              
        
        

        


