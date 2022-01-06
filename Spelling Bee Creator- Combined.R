options(warn = -1)
library(magrittr)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(scales)
options(warn = 0)

#### Inputs ####
L1 <- 1
L2 <- 16
List.Col <- 3
max.panagrams <- 4
min.score <- 100      
max.score <- 550


#### Part 1 ####
flag <- "Part 1"

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

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

words <- word.list %>% 
  filter(unique_letters == 7) %>% 
  select(word, letters, word_length, points) %>% 
  filter(!str_detect(word, "^.*ED$") ) %>% 
  filter(!str_detect(word, "^.*ING$") ) %>% 
  filter(!str_detect(word, "^.*LIKE$") ) %>%
  filter(!str_detect(word, "^.*IVE$") ) %>% 
  filter(!str_detect(word, "^ANTI.*$") ) %>% 
  filter(!str_detect(word, "^NON.*$") ) %>% 
  filter(between(word_length, L1, L2) )

words2 <- words %>% 
  filter(!str_detect(word, "^.*IZE$") ) %>% 
  filter(!str_detect(word, "^.*ER$") )

#####################################
# tester.maxx <- 25
# words2 <- words2[1:tester.maxx,]
#####################################

rm(L1, L2)
rm(word.url)

print("~ ~ ~ Part 1 Complete ~ ~ ~")


#### Part 2 ####
flag <- "Part 2"

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

print("~ ~ ~ Part 2 Complete ~ ~ ~")


#### Part 3 ####
flag <- "Part 3"

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

print("~ ~ ~ Part 3 Complete ~ ~ ~")


#### Part 4 ####
flag <- "Part 4"

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


print("~ ~ ~ Part 4 Complete ~ ~ ~")

#### Part 5 ####
flag <- "Part 5"

Ender.Col <- ncol(words5)
Starter.Col1 <- Ender.Col - 6
Starter.Col2 <- List.Col + 1

words.5.scores <- words5[,Starter.Col1:Ender.Col]

words.5.min <- apply(words.5.scores, 1, min)
words.5.max <- apply(words.5.scores, 1, max)

headers <- c( colnames(words5[,1:Starter.Col2]), 
              paste0("L", 1:7),
              "Minn", "Maxx")

words6 <- cbind(words5, words.5.min) %>% 
  cbind(., words.5.max) %>% 
  set_colnames(headers) %>% 
  filter(Maxx >= min.score) %>% 
  filter(Minn <= max.score)

rm(words.5.scores, words.5.min, words.5.max)
rm( min.score, max.score)
rm(headers)
rm(List.Col, Ender.Col, Starter.Col1, Starter.Col2)

print("~ ~ ~ Part 5 Complete ~ ~ ~")

rm(flag)


words7 <- words6 %>% 
    select(-"Letters")
cc(words7)