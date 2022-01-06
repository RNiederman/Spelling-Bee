options(warn = -1)
library(magrittr)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(scales)
options(warn = 0)

#### Inputs ####
List.Col <- 3


#### Part 4 ####

Words4 <- words4       

get.words <- function(center_letter, other_letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
  all_letters <- c(center_letter, other_letters)
  HC <- word.list %>%
    filter(str_detect(word, center_letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all_letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(HC)
}

L <- nrow(Words4)
words40 <- matrix(nrow = L, ncol = 7)

for (i in 1:L) {
  w <- Words4[i, List.Col] %>% 
    unlist %>% 
    unname
  
  for (j in 1:7) {
    
    center <- w[j]
    others <- setdiff(w, center)
    
    HC.Score <- get.words(center, others) %>% 
      pull(points) %>% 
      sum

    words40[i,j] <- HC.Score
  }

  perc <- i %>% divide_by(L) %>% multiply_by(100) %>% round(., 2) %>% paste0(., "%")
  print(paste0(comma(i), " / ", comma(L), "--> ", perc) )

}   

words5 <- cbind(Words4, words40)


rm(i, L, perc, j)
rm(w, center, others, HC.Score)
rm(Words4, words40)
