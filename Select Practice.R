load("data/concrete_abstract.Rdata")

library(dplyr)
library(readxl)

m_conc_abs <- m_conc_abs %>%
  arrange(WordType, level)

write.csv(m_conc_abs, "data/concrete_abstract.csv")


conc <- read_excel("data/Paxman_concreteness.xlsx") 
freq_context <- read_xlsx("data/SUBTLEX-US frequency list with PoS and Zipf information.xlsx")


conc <- left_join(conc, select(freq_context, Word, Dom_PoS_SUBTLEX))
conc <- conc %>%
  filter(!Word %in% m_conc_abs$Word,
         Dom_PoS_SUBTLEX == "Noun") 


z_abs <- sample(conc[conc$WordType == "Abstract",]$Word, 12)
z_conc <- sample(conc[conc$WordType == "Concrete",]$Word, 12)

practice_items <- rbind(conc %>% filter(Word %in% z_conc), conc %>% filter(Word %in% z_abs))

write.csv(practice_items, "data/practice_items.csv")
