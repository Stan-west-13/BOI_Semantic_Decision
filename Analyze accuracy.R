library(dplyr)
library(tidyr)
library(tidyverse)
d <- read.csv("data/BOI Semantic Decision_June 18, 2024_18.47.csv")[-c(1,2),]
load("data/wordList_orders.Rdata")


d_long <- d %>%
            pivot_longer(.,
                         cols = ends_with("Q1"),
                         values_to = "response",
                         names_to = "rorder") %>%
            filter(!rorder == "Q1") %>%
            mutate(rorder = str_sub(rorder,2,-1) )
d_long$rorder

df_list$list <- as.factor(df_list$list)


load("data/stim_600.Rdata")

samp_fun <- function(x,n_samp){
  y_abs <- x %>% filter(WordType == "Abstract")
  y_conc <- x %>% filter(WordType == "Concrete")
  abs_z <- sample(y_abs$word, n_samp)
  conc_z <- sample(y_conc$word, n_samp)
  return(rbind(x[x$word %in% abs_z, ],x[x$word %in% conc_z, ] ))
}

conc_abs_stim <- conc_abs_stim %>%
                    mutate(word = Word)

block1 <- samp_fun(conc_abs_stim,  50)
block2 <- conc_abs_stim %>% filter(!word %in% block1$word) %>% samp_fun(., 50)
block3 <- conc_abs_stim %>% filter(!word %in% block1$word, !word %in% block2$word) %>% samp_fun(., 50)
block4 <- conc_abs_stim %>% filter(!word %in% block1$word, !word %in% block2$word,!word %in% block3$word) %>% samp_fun(., 50)
block5 <- conc_abs_stim %>% filter(!word %in% block1$word, !word %in% block2$word,!word %in% block3$word,!word %in% block4$word) %>% samp_fun(., 50)
block6 <- conc_abs_stim %>% filter(!word %in% block1$word, !word %in% block2$word,!word %in% block3$word,!word %in% block4$word,!word %in% block5$word) %>% samp_fun(., 50)

write.csv(block1, "data/SDT_BOIblock1.csv")
write.csv(block2, "data/SDT_BOIblock2.csv")
write.csv(block3, "data/SDT_BOIblock3.csv")
write.csv(block4, "data/SDT_BOIblock4.csv")
write.csv(block5, "data/SDT_BOIblock5.csv")
write.csv(block6, "data/SDT_BOIblock6.csv")

block1_openlab <- block1 %>%
                    select(word, WordType) %>%
                    mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
                    select(-WordType)

block2_openlab <- block2 %>%
  select(word, WordType) %>%
  mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
  select(-WordType)


block3_openlab <- block3 %>%
  select(word, WordType) %>%
  mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
  select(-WordType)

block4_openlab <- block4 %>%
  select(word, WordType) %>%
  mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
  select(-WordType)

block5_openlab <- block5 %>%
  select(word, WordType) %>%
  mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
  select(-WordType)


block6_openlab <- block6 %>%
  select(word, WordType) %>%
  mutate(is_concrete = ifelse(WordType == "Concrete", TRUE, FALSE)) %>%
  select(-WordType)


write.csv(block1_openlab, "data/SDT_BOIblock1_openlab.csv")
write.csv(block2_openlab, "data/SDT_BOIblock2_openlab.csv")
write.csv(block3_openlab, "data/SDT_BOIblock3_openlab.csv")
write.csv(block4_openlab, "data/SDT_BOIblock4_openlab.csv")
write.csv(block5_openlab, "data/SDT_BOIblock5_openlab.csv")
write.csv(block6_openlab, "data/SDT_BOIblock6_openlab.csv")


button_feed <- data.frame(trials = rep(c("concrete","abstract"), each = 15))
button_feed <- button_feed %>%
                  mutate(rorder = rnorm(30)) %>%
                  arrange(rorder) %>%
                  select(-rorder)





prac <- read.csv("data/practice_items.csv")

practice_loop <- prac %>%
                  mutate(correct_response = ifelse(WordType == "Concrete", "true","false")) %>%
  select(Word, correct_response)


write.csv(practice_loop, "data/practice_items.csv")
