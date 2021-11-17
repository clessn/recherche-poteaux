#Library####
library(tidyverse)

#vote2011 <- readRDS("_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2011.rds")%>%
#  mutate(annee=2011)
vote2015 <- readRDS("_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2015.rds")%>%
  mutate(annee=2015)
vote2019 <- readRDS("_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2019.rds")%>%
  mutate(annee=2019)

vote <- rbind(vote2015, vote2019) %>%
  group_by(IdCirc, Parti, annee) %>%
  summarise(NbVotes=sum(NbVotes))

electeurs <- rbind(vote2015, vote2019) %>%
  group_by(IdCirc, annee, IdBureau) %>%
  summarise(NbÉlecteurs = mean(NbÉlecteurs)) %>%
  summarise(NbÉlecteurs=sum(NbÉlecteurs))

graphData <- left_join(
  vote,
  electeurs,
  by = c("IdCirc", "annee"))%>%
  mutate(pct = round(NbVotes/NbÉlecteurs*100, 2))%>%
  pivot_wider(names_from = annee, values_from=pct)%>%
  replace_na(list("2015"=0, "2019"=0))%>%
  group_by(IdCirc, Parti) %>%
  summarise(p2015 = sum(`2015`), 
            p2019 = sum(`2019`)) %>%
  filter(Parti != "autre") %>%
  mutate(absDiff = abs(p2015 - p2019))


# Remaniement électoral de 2012 ####
check2011 <- vote2011 %>%
  group_by(IdCirc) %>%
  summarise(nom2011 = unique(NomCirc_FR))
check2015 <- vote2015 %>%
  group_by(IdCirc) %>%
  summarise(nom2015 = unique(NomCirc_FR))
check2019 <- vote2019 %>%
  group_by(IdCirc) %>%
  summarise(nom2019 = unique(NomCirc_FR))

check <- left_join(check2019, check2015, by = "IdCirc") %>%
  left_join(., check2011, by = "IdCirc") %>%
  select(IdCirc, nom2011, nom2015, nom2019)

checkNAs <- left_join(check2019, check2015, by = "IdCirc") %>%
  left_join(., check2011, by = "IdCirc") %>%
  select(IdCirc, nom2011, nom2015, nom2019) %>%
  filter(is.na(nom2011))

#Colonne qui indique les circonscriptions qui ont
##               changé de nom entre 2011 et 2015
check$nameChange <- ifelse(check$nom2011 != check$nom2015, 1, 0)

write.csv(check, "_SharedFolder_RecherchePoteaux/vote/remaniement/check.csv")
