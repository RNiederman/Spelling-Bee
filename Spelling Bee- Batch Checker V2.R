options(warn = -1)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
options(warn = 0)

Proper <- function(s) {
  s1 <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)
  return(s1)
}

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

max.pgs <- 10


if (!exists("word.list")) {
    word.list <- read_lines(word.url, warn = FALSE) %>%
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


panagram.words <- function(center_letter, other_letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
  all_letters <- c(center_letter, other_letters)
  PG <- word.list %>%
    filter(unique_letters == 7) %>% 
    filter(str_detect(word, center_letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all_letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(PG)
}

combos <- words2$word

q <- length(combos)
cols <- 5 + max.pgs
M <- matrix(nrow = q, ncol = cols)

for (i in 1:q) {
  
  w <- combos[i] %>%
    toupper %>% 
    str_extract_all(., "[A-Z]") %>% 
    unlist %>% 
    unique 
  
  rando <- sample(7, 1)
  
  center <- w[rando]
  others <- setdiff(w, center)
  
  HC.words <- get.words(center, others)
  PG.words <- panagram.words(center, others) %>% 
    arrange(., points, word)
  
  score <- sum(HC.words$points)
  pg.count <- nrow(PG.words)
  g <- c(center, others) %>% 
    sort %>% 
    paste(., collapse = "")
  
  pgs <- PG.words$word %>% 
    Proper
  
  M[i, 1] <- i
  M[i, 2] <- combos[i]
  M[i, 3] <- g
  M[i, 4] <- score
  M[i, 5] <- pg.count
  
  pg.count <- min(pg.count, max.pgs) 
  
  for (k in 1:pg.count) {
    M[i, 5 + k] <- pgs[k]
  }
  
  print( paste(i, q, sep = " / ") )
  
}

rm(i, k, g, w)
rm(center, others)
rm(score, pgs, pg.count)
rm(HC.words, PG.words)
rm(cols, q)

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
cb(M)