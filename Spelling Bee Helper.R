options(warn = -1)
  library(XML)
  library(curl)
  library(magrittr)
  library(stringr)
  library(tibble)
  library(dplyr)
  library(purrr)
options(warn = 0)

clean.up <- TRUE
  
w <- "Idefgty" %>%
  toupper %>% 
  str_extract_all(., "[A-Z]") %>% 
  unlist %>% 
  unique 

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"
# word.url <- "http://www.mieliestronk.com/corncob_caps.txt"

center <- w[1]
others <- setdiff(w, center) %>% sample

stopifnot(length(w) == 7 )
stopifnot(length(setdiff(others, center) ) == 6)

if (!exists("word.list")) {
  word.list.raw <- curl(word.url)
  
  word.list <- word.list.raw %>% 
    readLines(., warn = FALSE) %>% 
    unlist %>%
    toupper %>%
    tibble %>% 
    transmute(., "word" = .) %>% 
    mutate(word.length = str_length(word) ) %>%
    filter(word.length >= 4) %>%
    filter(!grepl("\\W", word) ) %>%
    filter(!str_detect(word, "S")) %>%
    mutate(letters = str_split(word, ""), letters = map(letters, unique) ) %>%
    mutate(unique.letters = lengths(letters) ) %>%
    mutate(points = ifelse(word.length == 4, 1, word.length) + 7 * (unique.letters == 7) ) %>%
    filter(unique.letters <= 7) %>% 
    select(word, letters, word.length, points, unique.letters) %>% 
    arrange(desc(points))
  
    close(word.list.raw)
    rm(word.list.raw)
}

get.words <- function(center.letter, other.letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
  all.letters <- c(center.letter, other.letters)
  HC <- word.list %>%
    filter(str_detect(word, center.letter)) %>%
    mutate(invalid.letters = map(letters, setdiff, all.letters)) %>%
    filter(lengths(invalid.letters) == 0) %>%
    select(word, points)
  return(HC)
}


HC.Words <- get.words(center, others)

rm(word.url)

g01 <- others %>% 
  sort %>% 
  paste(., collapse = "") %>% 
  paste0(center, .) %>% 
  tolower

g02 <- c(center, others) %>% 
  sort %>% 
  paste(., collapse = "")


sum(HC.Words$points)
HC.Words %>% 
  filter(points >= 14) %>%
  nrow
# g01
# g02


if (clean.up) {
  rm(w, center, others)
  # rm(word.list)
  rm(g01, g02)
  # rm(HC.words)
}  
  
rm(clean.up)
