data <- read.csv("data/Semantic-Decision-Task-full-data.csv")
na_qual <- read.csv("data/BOI_Semantic_Decision_NA_July 1, 2024_12.05.csv")[-c(1,2),]
asd_qual <- read.csv("data/BOI_Semantic_Decision_Aut_July 1, 2024_12.05.csv")[-c(1,2),]

na_qual_g <- na_qual %>%
  mutate(group = "NA") %>%
  select(PROLIFIC_PID, group) %>%
  filter(!PROLIFIC_PID == "",! is.na(PROLIFIC_PID)) %>%
  unique()

asd_qual_g <- asd_qual %>%
  mutate(group = "ASD") %>%
  select(PROLIFIC_PID, group) %>%
  filter(!PROLIFIC_PID == "",! is.na(PROLIFIC_PID)) %>%
  unique()

groups <- rbind(asd_qual_g,na_qual_g)


library(dplyr)
create_PID <- function(x){
  x_pid <- x %>%
    filter(!is.na(openLabId)) %>%
    group_by(openLabId) %>%
    mutate(PROLIFIC_PID = str_match(meta[1],"PROLIFIC_PID=\\s*(.*?)\\s*&")[2] )
  return(x_pid)
}

data <- create_PID(data)

data <- data %>% 
          filter(!PROLIFIC_PID == "", !is.na(PROLIFIC_PID))

data <- left_join(data,groups)


d_filt <- data %>%
  filter(!is.na(trial_number), !trial_number == "",sender == "Trial") %>%
  select(openLabId,sender,ended_on,word,correct,response,trial_number, duration,PROLIFIC_PID,group) %>%
  unique() %>%
  mutate(PROLIFIC_PID = as.factor(PROLIFIC_PID),
         duration = as.numeric(duration)) %>%
  group_by(PROLIFIC_PID) %>%
  mutate(n_unknown = sum(response == "unknown",na.rm = T),
         n_correct = sum(correct == "true", na.rm = T),
         n_unanswered = sum(response =="",na.rm = T),
         n_incorrect = sum(correct == "false" & !response == "unknown", na.rm = T),
         perc_correct = sum(correct == "true", na.rm = T)/(600-(n_unknown + n_unanswered)),
         perc_total = sum(correct == "true",na.rm=T)/600,
         mean_dur = mean(duration,na.rm = T),
         total = n_unknown+n_correct+n_incorrect+n_unanswered,
         reject = ifelse(perc_correct < 0.5 | perc_total < 0.5, TRUE,FALSE)) %>%
  unique()

saveRDS(d_filt, "data/BOI-Semantic-Judgement-metadata.rds")








