options(warn = -1)
library(magrittr)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
options(warn = 0)

clean.up <- TRUE
  
w <- "Linemct" %>%
  toupper %>%
  str_extract_all(., "[A-Z]") %>%
  unlist %>%
  unique

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

center <- w[1]
others <- setdiff(w, center) %>% sample

stopifnot(length(w) == 7 )
stopifnot(length(setdiff(others, center) ) == 6)

if (!exists("word.list")) {
    word.list <- readLines(word.url, warn = FALSE) %>%
    toupper %>%
    tibble %>% 
    transmute(., "word" = .) %>% 
    mutate(word_length = str_length(word) ) %>%
    filter(word_length >= 4) %>%
    filter(!grepl("\\W", word) ) %>%
    filter(!str_detect(word, "S")) %>%
    mutate(letters = str_split(word, ""), letters = map(letters, unique) ) %>%
    mutate(unique_letters = lengths(letters) ) %>%
    mutate(points = ifelse(word_length == 4, 1, word_length) + 7 * (unique_letters == 7) ) %>%
    filter(unique_letters <= 7) %>% 
    select(word, letters, word_length, points, unique_letters) %>% 
    arrange(desc(points))
}

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

HC.words <- get.words(center, others)
PG.words <- panagram.words(center, others) %>% 
  arrange(., points, word)

rm(word.url)


g01 <- others %>% 
  sort %>% 
  paste(., collapse = "") %>% 
  paste0(center, .) %>% 
  tolower

g02 <- c(center, others) %>% 
  sort %>% 
  paste(., collapse = "")


sum(HC.words$points)
nrow(PG.words)
# g01
# g02

if (clean.up) {
rm(w, center, others, word.list)
rm(g01, g02)
# rm(HC.words, PG.words)
}  
  
rm(clean.up)