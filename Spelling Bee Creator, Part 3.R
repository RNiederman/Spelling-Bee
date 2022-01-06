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

#### Part 3 ####


panagram.words <- function(center.letter, other.letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
  all.letters <- c(center.letter, other.letters)
  PG <- word.list %>%
    filter(unique_letters == 7) %>% 
    filter(str_detect(word, center.letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all.letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(PG)
}

L <- nrow(Words3)
panagrams <- rep(NA, L)

for (i in 1:L) {
  
  w <- Words3[i, List.Col] %>% 
    unlist %>% 
    unname
  
  center <- w[1]
  others <- setdiff(w, center)
  
  PG.Count <- panagram.words(center, others) %>% 
    arrange(points) %>% 
    nrow

  panagrams[i] <- PG.Count

  perc <- i %>% divide_by(L) %>% multiply_by(100) %>% round(., 1) %>% paste0(., "%")
  print(paste0(comma(i), " / ", comma(L), "--> ", perc, " : ", PG.Count) )
  
}   

Words3Z <- Words3 %>% data.frame
panagrams2 <- panagrams %>% 
  data.frame %>% 
  set_colnames("Panagram.Count")

words4 <- cbind(Words3Z, panagrams2) %>% 
  data.frame %>% 
  filter(Panagram.Count <= max.panagrams)

rm(i, L, perc)
rm(w, center, others, PG.Count)
rm(max.panagrams)
rm(Words3Z, panagrams2)
rm(panagrams)
