
library("tidyverse")
library("tidymodels")

projdir <- getwd()
setwd("C:/Users/evand/Downloads")
raw <- read.csv2("bbga_latest_and_greatest.csv")
gebied <- read.csv2("gebied.csv")

setwd(projdir)
dataAms <- pivot_wider(raw,names_from = variabele,values_from = waarde)

dataAms[3:ncol(dataAms)] <- sapply(dataAms[3:ncol(dataAms)],as.numeric)
names(dataAms) <- names(janitor::clean_names(dataAms))

test <- merge(dataAms,gebied,by="gebiedcode15")

dataAms <- select(test,gebiednaam,gebiedcode15:skacti_1000inw)



unique(dataAms$gebiedcode15) # welke gebieden willen we uitkiezen? 
unique(dataAms$gebiednaam) # welke gebieden willen we uitkiezen? 

unique(select(dataAms, gebiedcode15, gebiednaam))


test<- dataAms %>% 
  filter(str_detect(gebiedcode15, "^1")) #4-cijferig postcodegebied?

unique(test$gebiedcode15) # check of de filter gewerkt heeft

dataAms <- test # overschrijf data
rm(test) # delete test databestand

# er staan veel NAs in, wat moten we weggooien?

na.omit(dataAms) # 0 rijen over, die kunnen we niet gebruiken.

test <- dataAms[, colSums(is.na(dataAms)) != nrow(dataAms)] # If the count of NAs in a 
#column is equal to the number of rows, it must be entirely NA.

dataAms <- test

# er staat altijd een waarde in de eerste twee kolommen (jaar en gebied). 
# Maar kunnen we ook rijen excluderen die voor de rest alleen NAs hebben?
dataAms[rowSums(is.na(dataAms)) != ncol(dataAms)-2, ] # Apply is.na function

unique(dataAms$bevgeb) # klopt er staan genoeg waardes in bijvoorbeeld deze

colSums(is.na(dataAms))

unique(dataAms$jaar)

names(dataAms)

# dat is leuk maar dat blijft er geen interessante uitkomstmaat over...

dataAms <- pivot_wider(raw,names_from = variabele,values_from = waarde)
dataAms[3:ncol(dataAms)] <- sapply(dataAms[3:ncol(dataAms)],as.numeric)
names(dataAms) <- names(janitor::clean_names(dataAms))

iww <- select(dataAms, gebiedcode15, jaar, iww, bevtotaal) %>% arrange(gebiedcode15) %>% na.omit

unique(iww$gebiedcode15)

filter(iww, gebiedcode15 != "STAD", gebiedcode15 != "A", gebiedcode15 != "E", 
       gebiedcode15 != "T", gebiedcode15 != "K", gebiedcode15 != "M", gebiedcode15 != "N", 
       gebiedcode15 != "Z", gebiedcode15 != "F", gebiedcode15 != "D")

dataAms$geb