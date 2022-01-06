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

if (!exists("word.list")) {
    wword.list <- read_lines(word.url, warn = FALSE) %>%
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
  require(purrr)
  require(dplyr)
  require(stringr)
  
  all_letters <- c(center_letter, other_letters)
  HC <- word.list %>%
    filter(str_detect(word, center_letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all_letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(HC)
}


panagram.words <- function(center_letter, other_letters) {
  require(purrr)
  require(dplyr)
  require(stringr)
  
  all_letters <- c(center_letter, other_letters)
  PG <- word.list %>%
    filter(unique_letters == 7) %>% 
    filter(str_detect(word, center_letter)) %>%
    mutate(invalid_letters = map(letters, setdiff, all_letters)) %>%
    filter(lengths(invalid_letters) == 0) %>%
    select(word, points)
  return(PG)
}

combos <-  c("raguity", "xrateic", "econlix", "clompit", "tribvey", "anutdrq", "revultc", "agodnbv", 
             "ovelbnt", "archmyp", "nehitvb", "caloity", "ochirky", "thickop", "toynjem", "lircapn", 
             "icarlny", "vurtace", "abtoney", "gcentay", "mryetix", "limezgt", "rquitec", "bloveya", 
             "uxmirte", "ftalcie", "heartfm", "omynarc", "othinml", "lovytar", "cyuanop", "ehymngo", 
             "owirdft", "iklotca", "ahempty", "letiaqu", "pomnext", "tanquio", "bamicen", "imroven", 
             "koberay", "idrytex", "imchunk", "fbandle", "abciour", "toplnak", "onutygl", "fnetaul", 
             "cmintle", "abourgl", "lortpuy", "anchopy", "citedfv", "mugtore", "mitnova", "iclmpty", 
             "brucile", "umondip", "utiprey", "etainox", "ecimzon", "omanyhg", "othymlg", "marvipe", 
             "echariy", "firtyna", "awolenc", "yanoter", "frapect", "copynet", "eturfpc", "avirypc", 
             "cnetpod", "bleachw", "domilar", "onticuy", "dramtoy", "elmobiz", "imclerk", "lyratuc", 
             "egothly", "glompay", "ecinluy", "robatcl", "nytliob", "bngolde", "tyrabiu", "baltirn", 
             "ridgmap", "ontulip", "awrebzy", "itrugly", "grackew", "mintcex", "vacehln", "elictyf", 
             "foberin", "pmailod", "ohlmrew", "raintmv", "aucomit", "mugloar", "goylied", "ramplon", 
             "validue", "owlgrny", "tpacefy", "apirdmy", "odubhln", "unitcof", "fdortil", "odwhink", 
             "engulfv", "mrpivet", "wirtzen", "kitebra", "dinkmog", "oghdurn", "itchmar", "hitparc", 
             "aridwth", "ldiytuf", "elbfixy", "cointlm", "caunitl", "aputchz", "katlive", "odrytim", 
             "eclopin", "nutiqap", "agileof", "ryecivo", "cubetrh", "kperabc", "coneymr", "montcab", 
             "bodniec", "elmburc", "ofindlb", "orutaic", "moxytan", "rfaminy", "opinftr", "adomlen", 
             "onibcue", "arungdv", "whodcer", "onaxter", "weldrib", "acotefi", "hnextar", "ryolgit", 
             "ihgdmnt", "azblidr", "hapcent", "elmpcox", "avonchy", "outwndc", "phanlet", "kbodrec", 
             "fecalun", "truncod", "kermaly", "brectax", "anhimpb", "tramnoh", "oriftlp", "troudpc", 
             "adeckbf", "yecantl", "pornfut", "apucity", "owlbfed", "vialmed", "naplift", "uhorgpt", 
             "goilart", "canemik", "trmacid", "crimode", "hotcend", "yimalon", "achruny", "naxgech", 
             "abuhilt", "ovencat", "hicpear", "ohbling", "luatriv", "bflatou", "odunght", "warblen", 
             "cabunde", "hclonat", "mlobaze", "vrnlace", "routfyi", "olhtmen", "tycalfu", "opinhld", 
             "otanmph", "mauitry", "embagty", "auntdjc", "ruganod", "nevilcb", "ochuber", "idletuy", 
             "clybear", "pcentry", "apyblok", "kredbay", "equinot", "cartque", "ezapbit", "elozcan", 
             "prizeta", "czenard", "klabten", "grobake", "brickea", "minkero", "mndtaco", "othuflg", 
             "ojcintu", "ucratlo", "getmail", "vindect", "clovite", "pelicab", "abynout", "iqbuter", 
             "tabquen", "aquilco", "untfreq", "antiquy", "ezmoral", "ezmblnt", "orlwhip", "owldeck", 
             "lodhike", "ridgopy", "ferpity", "irotaul", "poltner", "acivypt", "uregail", "baildot", 
             "creamny", "arimnuj", "imbethl", "amulfro", "darnicl", "reyquin", "geturia", "fedarop", 
             "orackub", "yparthe", "botrace")
q <- length(combos)
cols <- 9
M <- matrix(nrow = q, ncol = cols)

for (i in 1:q) {
  
  w <- combos[i] %>%
    toupper %>% 
    str_extract_all(., "[A-Z]") %>% 
    unlist %>% 
    unique 
  
  center <- w[1]
  others <- setdiff(w, center) %>% sample
  
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
  
  for (k in 1:pg.count) {
    M[i, 6 + k] <- pgs[k]
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