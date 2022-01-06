library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(magrittr)

'%!in%' <- function(x,y)!('%in%'(x,y))

current.keys <- c("ACEMNRY", "AILMNOY", "EIMPRTV", 
                  "ACEHLNV", "ADGNORU", "BCEINOU", "ABIRTUY", "ABENQTU", "ACIPRVY", "CENOPTY", "AHMNORT", 
                  "IMNOPRT", "EFNQRTU", "ACEGHLN", "ABCERTX", "CDEHORW", "ABGILNU", "CFINOTU", "AFIMNRY", 
                  "ACEINTY", "AINOQTU", "EIMOPTZ", "CIJNOTU", "FINOPRT", "BELMNTZ", "ACILNRY", "ACILNTU", 
                  "ACIKLOT", "ADGNOPR", "ACFLTUY", "BDHLNOU", "ALMNOPR", "ACILMTY", "EGLMNOU", "ADEHLRY", 
                  "ACILOTY", "AFLMORU", "ACEFIOT", "ABLOPRY", "ACEHIPR", "GHOPRTU", "CDEIMOR", "BGHILNO", 
                  "AEIPRTZ", "ELNOPRT", "EIMNORV", "ABCEORT", "BEIQRTU", "ACEGORV", "BELNOTV", "DFILORT", 
                  "ACFILTY", "AILOPTV", "AEGIMNZ", "ACMNORY", "EIMRTUX", "ABCERUY", "EINQRUY", "ACHNRUY", 
                  "EFIPRTY", "DEHIKLO", "CEFILNU", "ACHPTUZ", "GLNOTUY", "CEIJNOT", "ACIMOTU", "ABELNRW", 
                  "AEKMRTW", "ADNQRTU", "DEIRTXY", "BDEFLOW", "ABEGLTV", "ABERWYZ", "CEILPTX", "AEKOPRW", 
                  "AGILORT", "EGHMNOY", "ACHNOPY", "ADGILRU", "DEILTUY", "EGMORTU", "CDENOPT", "CDOPRTU", 
                  "CEGMNRY", "ACEFLNU", "AEHNRWY", "AILNOPV", "ABHLMOT", "AINQTUY", "ACEGOPR", "ACEGLOU", 
                  "ABELRVY", "EGMORTY", "ACLORTU", "ACEFGNR", "ABEGKOR", "ABGLORU", "AIMNOTV", "ABEJLNY", 
                  "ABDEFLN", "CEIKLMR", "ACLRTUY", "BDEILRW", "CDEINTV", "CEILMNT", "CIMNOTU", "CDNORTU", 
                  "ACDILNY", "AEIMNQU", "ALNOPTU", "ACHIMRT", "ABCENRY", "EINOQTU", "ADGIMPR", "CEMNORY", 
                  "EIMRTXY", "HILMNOT", "EGILOPU", "CEIMNOZ", "EGLOPRU", "AEFLNTU", "ACELNOZ", "ACDMNOT", 
                  "EINRTWZ", "ADEGMOU", "EGHLOTY", "AIJNORT", "AIJMNRU", "ACDEOTV", "CILMPTY", "AILRTUV", 
                  "BILNOTY", "ACEKLMR", "ACDENRZ", "BEHINTV", "ADILNUV", "BCELNOV", "GHLMOTY", "BEFINOR", 
                  "ACEGNTY", "ACHNOVY", "ACIPTUY", "CILMNOT", "ACDIMRT", "AEFGLOP", "GILORTY", "ABILNRT", 
                  "AIMNRTV", "ACILNPR", "CELMOPX", "BCEKOPT", "ACEQRTU", "CELRTUV", "CEGNORV", "ABCEKPR", 
                  "CEILNOP", "AEHNRTX", "ADILMOP", "ACEGHNX", "AIMRTUY", "CEIMNTX", "CINOTUY", "ACHMPRY", 
                  "CELNOPU", "EHLMORW", "CEILNOX", "ACEFILT", "ABCELRY", "BDEGLNO", "FINORTU", "BCEHRTU", 
                  "ACELNOW", "EINQTUY", "ADMORTY", "CDNOTUW", "AMNOTXY", "ABCIORU", "BEFILRY", "ACEIRTX", 
                  "ACNOPUY", "AEGHMOR", "ADEILUV", "AILOTVY", "ACMNOPY", "ACELNRV", "ABELMOZ", "AEFHMRT", 
                  "AEILQTU", "ACEFPRT", "AGLMOPY", "EGHILTW", "BEILMOZ", "BCELMRU", "DGIKMNO", "CILMOPT", 
                  "FGHLOTU", "FILOPRT", "ACILMNR", "ADHIRTW", "ABCKORU", "DGHNOTU", "AFGLNRT", "ACILNRV", 
                  "AHOPRTY", "ABINORW", "DGIOPRY", "EMNOPTX", "AEINOTX", "EKNORTW", "ADELMNO", "CEIORVY", 
                  "AEGIRTU", "CEFILTY", "CENPRTY", "AEMNPTV", "CEJOPRT", "EGILPRV", "ACEHNPT", "ACEHIRY", 
                  "ADEFHOR", "ACENOVY", "EHLMNOT", "ABCEIMN", "CEMNRTX", "ABDILRZ", "ABCDEFK", "BCEILNV", 
                  "ACELNTY", "CHIKMNU", "AINPQTU", "AFILNPT", "ADEILMV", "ACLMNOU", "ACENOTV", "ABEGMTY", 
                  "ADEFOPR", "ACHIPRT", "AEFGILO", "AENORTX", "AGIRTUY", "ACEIKMN", "DEGILOY", "BCEHORU", 
                  "BDFILNO", "GLNORWY", "ADINORY", "ABCELPU", "GILRTUY", "EIKMNOR", "AGHMNOY", "CDEFITV", 
                  "DHILNOP", "ADILNTW", "CEGHINY", "ADLNRUY", "ABCEILP", "CEILNUY", "EIPRTUY", "AEHLMPT", 
                  "DFILTUY", "EFGNORT", "AGLMORU", "CEHOPRY", "AEGNOPT", "AENORTY", "ACHILNP", "AGLNRTY", 
                  "CHIKOPT", "ABDEKRY", "BCDEKOR", "ACIPTVY", "ABCEHLW", "AEGILRU", "AFINRTY", "ILNOPTU", 
                  "ABNOTUY", "ACIMRTU", "ACDITUY", "EHILMOR", "ELNOQTU", "CEOQRTU", "AILRUXY", "ABENOTY", 
                  "AEHMPTY", "CDEHNOT", "LOPRTUY", "AILORTU", "CDEINPT", "BEFILXY", "DEILMTU", "ACILOQU", 
                  "ABDILOT", "BEIRTVY", "ABLMNOR", "DFIORTW", "DIMNOPU", "AEGILMT", "ABEGHLU", "DGHIMNT", 
                  "ACERTUV", "BCEILRU", "ACEFLTU", "ABGNORZ", "AIMNTUY", "CEIQRTU", "ABEIKRT", "EJMNOTY", 
                  "ACILQTU", "ABFLOTU", "ACIORTU", "ACEHLMY", "CEFPRTU", "EGILMTZ", "ABHIMNP", "ADILMOR", 
                  "ACEGKRW", "ADLMORU", "ABEKORY", "ACDILNR", "DIMORTY", "ACFILNU", "ABEKLNT", "FIORTUY", 
                  "CEHNORV", "ACDJNTU", "AEHPRTY", "ABCDENU", "ABCEIKR", "HILOPRW", "ADIKLOR", "ABHMORT", 
                  "DHIKNOW", "AHMNOPT", "ALORTVY", "CDEKLOW", "ACLNPTU", "LMOPRTY", "ACHLNOT", "AEIKLTV", 
                  "AKLNOPT", "AEHLNPT", "ACEFPTY", "BEHILMT", "ABCLORT", "ABELOVY", "ACNOPTU", "DGHNORU", 
                  "EFGLNUV", "AELMORZ", "ABCMNOT", "ABDGNOV", "BDELOUZ", "CHIKORY", "ABEIPTZ", "AEIMOTV", 
                  "EHLNOPT", "BCDEINO", "ADIMPRY", "CEILOTV", "ABHILTU", "BCDEHIR", "ADGNRUV", "AEIMPRV", 
                  "ABKLOPY", "AEKLMRY", "ABEINOR")


headers <- c("n", "word", "key", 
             "points", "panagrams", 
             paste0("word", 1:10))

lo <- 200
hi <- 800

df1 <- M %>% 
  data.frame %>% 
  set_colnames(headers) %>% 
  mutate(word_length = str_length(word) ) %>%
  filter(key %!in% current.keys) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate_at(c("points", "panagrams"), as.numeric) %>%
  filter(panagrams <= 2) %>% 
  filter(between(points, lo, hi)) %>% 
  filter(word_length > 7) %>%
  filter(!str_detect(word, "MEGA") ) %>% 
  filter(!str_detect(word, "ULTRA")) %>% 
  filter(!str_detect(word, "^.*MEN$") ) %>% 
  arrange(points, panagrams)