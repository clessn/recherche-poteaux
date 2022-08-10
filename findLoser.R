# library

library(tidyverse)
library(haven)
library(xlsx)


DataAll <- read_dta("/Users/alexandrecote/Dropbox/_CLESSN/recherche-poteaux/_SharedFolder_RecherchePoteaux/FinalCandidateData.dta")

DataNoTwitter <- read.csv2("/Users/alexandrecote/Dropbox/_CLESSN/recherche-poteaux/_SharedFolder_RecherchePoteaux/WinnersWithoutTwitter.csv")

names(DataNoTwitter)[names(DataNoTwitter) == 'name'] <- 'ndistrictname'

CleanData <- data.frame()


# for all candidates
CleanData <- merge(x = DataNoTwitter, y = DataAll, by = "ndistrictname", all = TRUE)

# for winner with  no twitter
#### MISS ####
# St.John's South--MountPearl
# Hamilton East--Stoney Creek

CleanDataNoTwitter <- left_join(DataNoTwitter, DataAll, by = "ndistrictname") %>% 
  filter(winner == 1) %>% 
  filter(eyear == 2019) 

write_csv(CleanDataNoTwitter, "/Users/alexandrecote/Dropbox/_CLESSN/recherche-poteaux/_SharedFolder_RecherchePoteaux/NoTwitter.csv")

CleanDataNoTwitter$age[(CleanDataNoTwitter$age == 99)] <- NA
CleanDataNoTwitter <- CleanDataNoTwitter %>% 
  drop_na(age)

CleanDataNoTwitter$meanAge <- mean(as.numeric(CleanDataNoTwitter$age))

#### 56 ans ###





CleanData <- CleanData %>% 
  filter(winner == 1) %>% 
  filter(eyear == 2019)

CleanData$age[(CleanData$age == 99)] <- NA
CleanData <- CleanData %>% 
  drop_na(age)

CleanData$meanAge <- mean(as.numeric(CleanData$age))









