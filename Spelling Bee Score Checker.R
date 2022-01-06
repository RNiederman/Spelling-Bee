options(warn = -1)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
options(warn = 0)

word.url <- "https://norvig.com/ngrams/enable1.txt"
# word.url <- "http://www-personal.umich.edu/~jlawler/wordlist"

if (!exists("word.list")) {
    word.list <- read_lines(word.url, warn = FALSE) %>%
    toupper %>%
    tibble(., "word" = . ) %>%
    mutate(word_length = str_length(word) ) %>%
    filter(word_length >= 4) %>%
    mutate(letters = str_split(word, ""), letters = map(letters, unique) ) %>%
    mutate(unique_letters = lengths(letters) ) %>%
    mutate(points = ifelse(word_length == 4, 1, word_length) + 7 * (unique_letters == 7) ) %>%
    filter(unique_letters <= 7) %>%
    arrange(desc(points))
}

get.words <- function(center_letter, other_letters) {
  require(magrittr)
  require(stringr)
  require(purrr)
  require(dplyr)
  
  all_letters <- c(center_letter, other_letters)
  word.list %>%
    filter(str_detect(word, center_letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all_letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
}

sb.set <- c("odubhln", "quirtec", "achruny", "yanoter", "cmintle", "bngolde", "icarlny", "cyuanop", 
            "pelmcox", "mugloar", "cpikeha", "pmailod", "ghotfid", "etainox", "hicpear", "paidmry", 
            "nutiqap", "ldiytuf", "imroven", "dhonkwi", "lircapn", "yecantl", "glompay", "inoubce", 
            "kermaly", "raintmv", "whodcer", "kbodrec", "caunitl", "goylied", "bflatou", "imclerk", 
            "abourgl", "owirdft", "wirtzen", "domilar", "moxytan", "goilart", "aucomit", "umondip", 
            "wrayabe", "ughdorn", "ptaciuy", "gtruily", "cnetpod", "ftalcie", "echariy", "vcaneto", 
            "cointlm", "hapcent", "oagexnh", "validue", "fdortil", "buihalt", "azblidr", "fnetaul", 
            "mapetri", "urotfyi", "poldate", "fturepc", "tribvey", "eclopin", "vrnlace", "hotcend")

z7 <- nchar(sb.set) %>% unique
stopifnot( z7 == 7 )

L <- length(sb.set)
score <- rep(NA, L)
panagrams <- rep(NA, L)
checker <- rep(NA, L)


for (i in 1:L) {
  W <- sb.set[i] %>% 
    toupper %>% 
    str_extract_all(., "[A-Z]") %>% 
    unlist %>% 
    unique
  center <- W[1]
  others <- setdiff(W, center)
  
  PG.words <- word.list %>%
    filter(str_detect(word, center))
  
  for (j in 1:length(others)) {
    PG.words <- PG.words %>%
      filter(str_detect(word, others[j] ) )
  }
  
  HC.words <- get.words(center, others)
  
  s <- sum(HC.words$points)
  p <- nrow(PG.words)
  
  g <- W %>% 
    sort %>% 
    paste(., collapse = "")
  
  score[i] <- s
  panagrams[i] <- p
  checker[i] <- g
  
  print(paste(i, L, sep = " / "))
}

sb.df <- cbind(sb.set, score, panagrams, checker) %>% 
  data.frame

checker %>% 
  unique %>% 
  length
