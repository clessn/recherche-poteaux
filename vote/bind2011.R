library(tidyverse)

Data <- 
  do.call(rbind,
          lapply(paste0("_SharedFolder_RecherchePoteaux/vote/vote2011/csvs_vote2011/",
                        list.files(path = "_SharedFolder_RecherchePoteaux/vote/vote2011/csvs_vote2011/")),
                 read.csv))

names(Data) <- c("IdCirc", "NomCirc_EN", "NomCirc_FR", "IdBureau", "NomBureau", "Indicateur1",
                 "Indicateur2", "FusionnéAvec", "BulletinsRejetés", "NbÉlecteurs", "NomFamille",
                 "MiddleName", "Prénom", "Parti_EN", "Parti_FR", "CandidatSortant", "CandidatÉlu",
                 "NbVotes")

enco <- c("NomCirc_EN", "NomCirc_FR", "NomBureau", "NomFamille",
          "MiddleName", "Prénom", "Parti_EN", "Parti_FR")

for (col in enco){
  Encoding(Data[[col]]) <- "UTF-8"}

Encoding(Data$Prénom) <- "UTF-8"
Encoding(Data$Parti_EN) <- "UTF-8"
Encoding(Data$Parti_FR) <- "UTF-8"

Data$Parti <- "autre"
Data$Parti[Data$Parti_EN == "Conservative"] <- "PCC"
Data$Parti[Data$Parti_EN == "Green Party"] <- "PVC"
Data$Parti[Data$Parti_EN == "Liberal"] <- "PLC"
Data$Parti[Data$Parti_EN == "NDP-New Democratic Party"] <- "NPD"
Data$Parti[Data$Parti_EN == "Parti populaire"] <- "PPC"
Data$Parti[Data$Parti_EN == "Bloc Qu\xe9b\xe9cois"] <- "BQ"

Data$CandidatSortant <- ifelse(Data$CandidatSortant == "Y", 1, 0)
Data$CandidatÉlu <- ifelse(Data$CandidatÉlu == "Y", 1, 0)

Data <- Data %>%
  select(c(IdCirc, NomCirc_FR, Parti, NbVotes, NbÉlecteurs, NomFamille, Prénom,
           CandidatSortant, CandidatÉlu, IdBureau, NomBureau))

saveRDS(Data, "_SharedFolder_RecherchePoteaux/vote/vote2011/ResultatsParBureau2011.rds")
saveRDS(Data, "_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2011.rds")
