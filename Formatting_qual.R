library(dplyr)
d <-readRDS("data/BOI-Semantic-Judgement-metadata.rds")
qual <- readRDS('data/ADC_ASSERT-BOI.rds')

ppids <- readRDS("data/Accuracy_cutoffs_ppids.rds")

d2 <- d %>%
  filter(PROLIFIC_PID %in% ppids)


qual_na <- na %>%
  select(PROLIFIC_PID, RecordedDate, Finished, Aut_Group,starts_with("Q24"),starts_with("Q25"), Q29, Q1.1, Q2,Q3.1,Q4.1,Q5.1,Q6.1,Q7)

qual_asd <- aut%>%
  select(PROLIFIC_PID, RecordedDate, Finished, Aut_Group,starts_with("Q24"),starts_with("Q25"), Q29, Q1.1, Q2,Q3.1,Q4.1,Q5.1,Q6.1,Q7)

d3 <- d2 %>%
  left_join(qual) 


d4 <- d3 %>%
  select(-openLabId,
         -sender,
         -reject,
         -RecordedDate,
         -Finished,
         -last_entry,
         -total) %>%
  select(PROLIFIC_PID, everything())


saveRDS(d4, file = "data/BOI-Assert-ADC_metadata.rds")

