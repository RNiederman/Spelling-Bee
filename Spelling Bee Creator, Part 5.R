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
min.score <- 100      
max.score <- 550


#### Part 5 ####


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
rm(min.score, max.score)
rm(headers)
rm(List.Col, Ender.Col, Starter.Col1, Starter.Col2)


words7 <- words6 %>% 
    select(-"Letters")


cc <- function(x, row.names = FALSE, col.names = TRUE, ...) 
{write.table(x, file = paste0("clipboard-", 2^19), sep = "\t", 
             row.names = row.names, col.names = col.names, na = "", ...)}

cc(words7)