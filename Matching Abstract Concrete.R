library(dplyr)
library(MatchIt)

load("data/concrete_abstract.Rdata")

stim_long <- pivot_longer(m_conc_abs,
                          cols = c("AoA_Kup_lem", "Lg10WF", "Lg10CD"),
                          names_to = "measure",
                          values_to = "value")


conc_matching <- stim_long %>%
  filter(WordType == "Concrete")


conc_matching_split <- split(conc_matching, conc_matching$measure)


t_test_lm <- map(conc_matching_split, function(x){
  return(t.test(value ~ level, data = x, subset = level == c("low", "medium")))
})


t_test_hl <- map(conc_matching_split, function(x){
  return(t.test(value ~ level, data = x, subset = level == c("low", "high")))
})

t_test_mh <- map(conc_matching_split, function(x){
  return(t.test(value ~ level, data = x, subset = level == c("high", "medium")))
})


split_abs_conc_matching <- split(stim_long, stim_long$measure)

t_test_abs_conc <- map(split_abs_conc_matching, function(x){
  return(t.test(value~WordType, data = x))
})




