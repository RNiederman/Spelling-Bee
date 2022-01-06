options(warn = -1)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
options(warn = 0)

#### Inputs ####
L1 <- 1
L2 <- 16


word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

#### Part 1 ####

word.list <- readLines(word.url, warn = FALSE) %>% 
    toupper %>%
    tibble %>% 
    transmute(., "word" = .) %>% 
    mutate(word_length = str_length(word) ) %>%
    filter(word_length >= 4) %>%
    filter(!str_detect(word, "S")) %>%
    mutate(letters = str_split(word, ""), letters = map(letters, unique) ) %>%
    mutate(unique_letters = lengths(letters) ) %>%
    mutate(points = ifelse(word_length == 4, 1, word_length) + 7 * (unique_letters == 7) ) %>%
    filter(unique_letters <= 7) %>% 
    arrange(word)


words <- word.list %>% 
  filter(unique_letters == 7) %>% 
  select(word, letters, word_length, points) %>% 
  filter(!str_detect(word, "^.*ED$")) %>% 
  filter(!str_detect(word, "^.*ING$")) %>% 
  filter(!str_detect(word, "^.*LIKE$") ) %>%
  filter(!str_detect(word, "^.*IVE$") ) %>% 
  filter(!str_detect(word, "^ANTI.*$") ) %>% 
  filter(!str_detect(word, "^NON.*$") ) %>% 
  filter(between(word_length, L1, L2) ) %>%
  sample_frac(1)

words2 <- words %>% 
  filter(!str_detect(word, "^.*IZE$") ) %>% 
  filter(!str_detect(word, "^.*ER$")) %>% 
  sample_frac(1)


rm(L1, L2)
# rm(word.list0)


# filter(str_detect(word, Q) ) 
# filter(str_detect(word, "K")) %>% 
# filter(!str_detect(word, "CK")) %>% 
