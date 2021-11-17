# Packages ####
library(tidyverse)

# Raw data ####
vote2015 <- readRDS("_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2015.rds")%>%
  mutate(annee=2015)
vote2019 <- readRDS("_SharedFolder_RecherchePoteaux/vote/parannee/ResultatsParBureau2019.rds")%>%
  mutate(annee=2019)

# Bind 2015 et 2019 ####
vote <- rbind(vote2015, vote2019) %>%
  group_by(IdCirc, Parti, annee) %>%
  summarise(NbVotes=sum(NbVotes))

## Nb d'électeurs par circonscription-année ####
electeurs <- rbind(vote2015, vote2019) %>%
  group_by(IdCirc, annee, IdBureau) %>%
  summarise(NbÉlecteurs = mean(NbÉlecteurs)) %>%
  summarise(NbÉlecteurs=sum(NbÉlecteurs))

# Circ-Parti-2015-2019-Diff ####
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


# Analyser la différence entre 2015-2019 ####
ggplot(graphData, aes(x = absDiff)) +
  geom_histogram(fill = "dodgerblue4", binwidth = 0.5) +
  xlab("Différence absolue du % de votes") +
  ylab("Nombre de combinaisons\nparti-circonscription\n") +
  ggtitle("Distribution de la différence absolue du % de votes par\ncirconscription pour chaque parti entre 2015 et 2019") +
  theme_bw()

ggsave("_SharedFolder_RecherchePoteaux/vote/graphs/absdiff.png")

interGraph <- graphData %>%
  mutate(p5 = ifelse(absDiff <= 5, 1, 0),
         p10 = ifelse(absDiff <= 10, 1, 0))

table(interGraph$p5)[2]/(table(interGraph$p5)[1] + table(interGraph$p5)[2])
table(interGraph$p10)[2]/(table(interGraph$p10)[1] + table(interGraph$p10)[2])

# Ajouter nos variables ####

## Trend 2015-2019 ####
main <- graphData %>%
  mutate(Trend1519 = p2019 - p2015,
         prov = substr(IdCirc, 1, 2))

  #graph
  main %>%
  group_by(Parti, prov) %>%
  summarise(Diff = sum(Trend1519)) %>%
  ggplot(aes(x = prov, y = Diff)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Parti)

## Moyenne pondérée ####
main2 <- main %>%
    mutate(MoyPond = 0.65*p2019 + 0.35*p2015)

## Différence avec le support national du parti ####
elecs_2015 <- electeurs %>%
    filter(annee == 2015)
NbElecteurs2015 <- sum(elecs_2015$NbÉlecteurs)

elecs_2019 <- electeurs %>%
  filter(annee == 2019)
NbElecteurs2019 <- sum(elecs_2019$NbÉlecteurs)
  
parti_national <- left_join(
    vote,
    electeurs,
    by = c("IdCirc", "annee"))%>%
    mutate(pct = round(NbVotes/NbÉlecteurs*100, 2)) %>%
    group_by(Parti, annee) %>%
    summarise(NbVotes = sum(NbVotes)) %>%
    mutate(NbElecteurs = ifelse(annee == 2015, NbElecteurs2015, NbElecteurs2019),
           SupportNational = round(NbVotes/NbElecteurs*100, 2))

