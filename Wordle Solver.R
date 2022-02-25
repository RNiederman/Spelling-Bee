library(hunspell)

black <- ""
exclude <- function(local, global = black) {
  LETTERS[!LETTERS %in% c(local, global)]
}
wordle <- function(one, two, three, four, five, include = "") {
  words <- apply(expand.grid(one, two, three, four, five), 1, \(x) paste(x, collapse = ""))
  words <- words[grepl(paste0("(?=.*", include, ")", collapse = ""), words, perl = TRUE)]
  words[hunspell_check(words)]
}

black <- c("E", "R", "T", "U", "A", "S", "G", "H", "K", "L", "C", "N")
wordle(one = exclude(""), 
       two = "I",
       three = exclude(""),
       four = "I",
       five = exclude(""),
       include = c("")
)