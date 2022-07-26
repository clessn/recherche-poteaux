# Packages ####
library(tidyverse)

# Raw data ####
vote2015 <- readRDS("_SharedFolder_RecherchePoteaux/parannee/ResultatsParBureau2015.rds")%>%
  mutate(annee=2015)
vote2019 <- readRDS("_SharedFolder_RecherchePoteaux/parannee/ResultatsParBureau2019.rds")%>%
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
  mutate(absDiff = abs(p2015 - p2019),
         diff = p2015-p2019)


# Variance 2015-2019 ####
ggplot(graphData, aes(x = absDiff)) +
  geom_histogram(fill = "dodgerblue4", binwidth = 1) +
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

# Qu'est-ce que ça nous apprend?
### 72% du temps, un parti varie de moins de 5%
### 90% du temps, un parti varie de moins 10%
### On peut donc inférer qu'un parti peut espérer avoir une croissance de maximum 10% à moins d'un contexte particulier dans la circonscription
### Donc, on peut quantifier (granulairement?) la chance d'un parti de gagner en plaçant une distribution des chances de variation autour du résultat de 2019

ggplot(graphData, aes(x = diff)) +
  geom_histogram(fill = "dodgerblue4", binwidth = 1) +
  xlab("Différence du % de votes") +
  ylab("Nombre de combinaisons\nparti-circonscription\n") +
  ggtitle("Distribution de la différence du % de votes par\ncirconscription pour chaque parti entre 2015 et 2019") +
  theme_bw()

table(round(graphData$diff))

DiffDist <- graphData %>%
  mutate(diff = round(diff)) %>%
  group_by(diff) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop/sum(prop))
  

Data <- data.frame(IdCirc = as.numeric(),
                   Parti = as.character(),
                   p2021 = as.numeric(),
                   prop = as.numeric())

for (i in 1:nrow(graphData)){
  #i <- 1
  circ <- as.numeric(graphData[i, 1])
  parti <- as.character(graphData[i, 2])
  p2019 <- as.numeric(graphData[i, 4])
  tobind <- DiffDist %>%
    mutate(p2021 = round(p2019) + diff,
           p2021 = ifelse(p2021 < 0, 0, p2021)) %>%
    group_by(p2021) %>%
    summarise(prop = sum(prop)) %>%
    cbind(data.frame(IdCirc = rep(circ, nrow(.)), Parti = rep(parti, nrow(.))), .)
  Data <- rbind(Data, tobind)
  if (i %% 10 == 0){
    print(i)
  }
}

colors <- c("PLC"    = "#FF0024",
            "PCC"    = "#099FFF",
            "NPD"    = "#FF6600",
            "BQ"     = "#00FFFF",
            "PVC"    = "#00FF00",
            "PPC"    = "#9D00FF")


Data %>%
  filter(IdCirc == unique(IdCirc)[sample(length(unique(IdCirc)), 1)]) %>%
  mutate(prop = prop*100) %>%
  ggplot(., aes(x = p2021, y = prop)) +
    geom_bar(stat = "identity", aes(group = Parti, fill = Parti, color = Parti),
             alpha = 0.6) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors)

0.05*0.05


# Ajouter nos variables ####










# Trend 2015-2019 ####
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

