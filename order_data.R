library(tidyverse)
library(openxlsx)

#for openwork
#data_work <- read.csv("./data_woman.csv")
data_work <- fromJSON("./data_gap.json")
data_work <- data_work[,-2]
element <- data_work$element
elements <- strsplit(element, "、")

job_ana <- c("Analyst", "analyst", "アナリスト")
job_srana <- c("Senior Analyst", "senior analyst", "Sr Analyst", "Sr.Analyst", "Sr. analyst", "Sr. Analyst", "シニアアナリスト")
job_cons <- c("Consultant", "consultant", "Cons", "コンサルタント")
job_srcons <- c("Senior Consultant", "Sr Consultant", "Sr.Consultant", "Sr. Consultant", "シニアコンサルタント")
job_mng <- c("Manager",  "マネージャー", "マネジャー")
job_srmng <- c("Senior Manager", "Sr Manager", "Sr.Manager", "Sr. Manager", "シニアマネージャー", "シニアマネジャー")
job_gpmng <- c("Group Manager", "group maneger",  "グループマネージャー")

role_list <- c(job_ana, job_srana, job_cons, job_srcons, job_mng, job_srmng, job_gpmng)

other_elements <- c()
job_state <- c()
role_series <- c()

for(e in elements){
  matched_e <- e %in% role_list
  if(any(matched_e == TRUE)){
    #print(e[matched_e])
    #exclude many TRUE
    if (sum(matched_e) != 1){
      cons_fact <- e %in% job_cons
      matched_e <- matched_e != cons_fact
    }
    role_series <- cbind(role_series, e[matched_e])
    other_elements <- cbind(other_elements, paste(e[!matched_e], collapse=","))
  }else{
    role_series <- cbind(role_series, "-")
    other_elements <- cbind(other_elements, paste(e, collapse=","))
  }
  current_state <- grep(pattern="(現職)|(退社)", e)
  if(length(current_state) != 0){
    job_state <- cbind(job_state, e[current_state])
  }else{
    job_state <- cbind(job_state, "unknown")
  }
}

result_data <- data_work
result_data$element <- as.vector(role_series)
paste(tmp, collapse=",")
names(result_data)[1] <- "job_title"

result_data$other_elements <- as.vector(other_elements)
result_data$state <- as.vector(job_state)

#unify notation
for (j in 1:length(result_data$job_title)){
  job_tmp <- result_data$job_title[j]
  if (!job_tmp %in% role_list){
    
  } else if (job_tmp %in% job_ana){
    result_data$job_title[j] <- "Analyst"
  } else if (job_tmp %in% job_srana){
    result_data$job_title[j] <- "Sr.Analyst"
  } else if (job_tmp %in% job_cons){
    result_data$job_title[j] <- "Consultant"
  } else if (job_tmp %in% job_srcons){
    result_data$job_title[j] <- "Sr.Consultant"
  } else if (job_tmp %in% job_mng){
    result_data$job_title[j] <- "Manager"
  } else if (job_tmp %in% job_srmng){
    result_data$job_title[j] <- "Sr.Manager"
  } else if (job_tmp %in% job_gpmng){
    result_data$job_title[j] <- "Group Manager"
  } 
}

#write_csv(result_data, "result_data.csv")
write.xlsx(result_data, "result_data.xlsx")