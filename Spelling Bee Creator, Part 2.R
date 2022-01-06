options(warn = -1)
library(magrittr)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(scales)
options(warn = 0)

#### Inputs ####
max.panagrams <- 4


#### Part 2 ####

words2a <- words2
# Words2a <- words

Words3z <- words2a %>% pull(word)
Words3a <- words2a %>% select(letters)

Words3b <- apply(Words3a, 1, function(x) unlist(x) %>%  
                   sort %>% 
                   unique %>% 
                   paste(., collapse =  "")
) 

Words3 <- cbind(Words3z, Words3b) %>% 
  data.frame %>% 
  set_colnames(c("Word", "Dummy.Word")) %>% 
  arrange(Dummy.Word) %>% 
  group_by(Dummy.Word) %>% 
  mutate(RN = row_number()) %>% 
  filter(RN == 1) %>% 
  mutate(Letters = str_split(Dummy.Word, "") ) %>% 
  select(Word, Dummy.Word, Letters)

rm(words2a, Words3a, Words3b, Words3z)
