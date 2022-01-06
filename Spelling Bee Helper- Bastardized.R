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
  
w <- "Geticastr" %>%
  toupper %>% 
  str_extract_all(., "[A-Z]") %>% 
  unlist %>% 
  unique 

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

center <- w[1]
others <- setdiff(w, center) %>% sample

Q.2 <- length(w)

# stopifnot(length(w) == 7 )
# stopifnot(length(setdiff(others, center) ) == 6)

word.list.raw.2 <- curl(word.url)
  
word.list.2 <- word.list.raw.2 %>% 
  readLines(., warn = FALSE) %>% 
  unlist %>%
  toupper %>%
  tibble %>% 
  transmute(., "word" = .) %>% 
  mutate(word.length = str_length(word) ) %>%
  filter(word.length >= Q.2) %>%
  filter(!grepl("\\W", word) ) %>%
  mutate(letters = str_split(word, ""), letters = map(letters, unique) ) %>%
  mutate(unique.letters = lengths(letters) ) %>%
  mutate(points = ifelse(word.length == 4, 1, word.length) + 7 * (unique.letters == Q.2) ) %>%
  filter(unique.letters <= Q.2) %>% 
  select(word, letters, word.length, points, unique.letters) %>% 
  arrange(desc(points))
  
close(word.list.raw.2)
rm(word.list.raw.2)

panagram.words.2 <- function(center.letter, other.letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
    all.letters <- c(center.letter, other.letters) %>%
    unique
  Q.22 <- length(all.letters)
  
  PG <- word.list.2 %>%
    filter(unique.letters == Q.22) %>% 
    filter(str_detect(word, center.letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all.letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(PG)
}


PG.Words <- panagram.words.2(center, others)
nrow(PG.Words)


if (clean.up) {
  rm(w, center, others)
  rm(word.url, word.list.2)
  rm(Q.2)
# rm(PG.words)
}  
  
rm(clean.up)