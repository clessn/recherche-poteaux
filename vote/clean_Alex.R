#Library####
library(tidyverse)

# Vote 2019 selon parti et circonscription ####

Df_data <- read_rds(file = "_SharedFolder_RecherchePoteaux/vote2019/ResultatsParBureau2019.rds")
Df_data$IdBureau <- paste0(Df_data$IdCirc, Df_data$IdBureau)

test <- Df_data %>% 
  group_by(IdCirc, Parti)%>%
  summarise(NbVotes = sum(NbVotes))

test1 <- Df_data %>% 
  group_by(IdCirc, IdBureau) %>%
  summarise(NbÉlecteurs = mean(NbÉlecteurs)) %>%
  summarise(NbÉlecteurs = sum(NbÉlecteurs))

vote <- left_join(test, test1, by = "IdCirc") %>% 
  mutate(PCT= round(NbVotes/NbÉlecteurs*100, digits = 1))
  


# Données twitter ####
twitter <- readRDS("_SharedFolder_RecherchePoteaux/vote2019/twitter.rds") %>%
  select(c(IdCirc = ED_CODE, Parti = data.currentParty,
           n_tw, has_twi))

twitter$Parti[twitter$Parti == "CPC"] <- "PCC"
twitter$Parti[twitter$Parti == "LPC"] <- "PLC"
twitter$Parti[twitter$Parti == "NDP"] <- "NPD"
twitter$Parti[twitter$Parti == "GPC"] <- "PVC"

# Merge vote-twitter et graph ####
merge <- left_join(twitter, vote, by = c("IdCirc", "Parti")) %>%
  replace_na(list(PCT = 0))

#Ajout max####

max <- merge %>% 
  group_by(IdCirc) %>%
  summarise(max = max(PCT))

# Merge vote twitter et graph + max ####

mergemax <- left_join(merge, max, by = c("IdCirc")) %>%
  mutate(dif = PCT - max)

#les zero tweets ####

zerotweet <- mergemax %>%
  mutate(logntw = log(n_tw)) %>%
  filter(logntw <= 0)

  


colors <- c("PLC"    = "#FF0024",
            "PCC"    = "#099FFF",
            "NPD"    = "#FF6600",
            "BQ"     = "#00FFFF",
            "PVC"    = "#00FF00",
            "PPC"    = "#9D00FF")


#Raw ####
ggplot(merge, aes(x = log(PCT), y = log(n_tw))) +
  geom_point(aes(color = Parti), size = 1) +
  geom_smooth(se = T, size = 1) +
  scale_color_manual(values = colors) +
  ylab("Nombre de tweets du candidat pendant\nla campagne électorale de 2021 (log)\n") +
  xlab("\nPerformance du parti dans la circonscription en 2019 en % (log)") +
  facet_wrap(facets = ~Parti, scales = "free") +
  theme_bw(base_size = 7.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


#Avec dif ####

ggplot(mergemax, aes(x = dif, y = log(n_tw))) +
  geom_point(aes(color = Parti), size = 1) +
  geom_smooth(se = F, size = 1) +
  scale_color_manual(values = colors) +
  ylab("Nombre de tweets du candidat pendant\nla campagne électorale de 2021 (log)\n") +
  xlab("\nDifférence avec le parti gagnant dans la circonscription en 2019 (log)") +
  facet_wrap(facets = ~Parti, scales = "free") +
  theme_bw(base_size = 7.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


ggsave("_SharedFolder_RecherchePoteaux/vote2019/avecdif.png")

#les zeros 

ggplot(zerotweet, aes(x = dif)) +
  geom_histogram(aes(fill = Parti)) +
  scale_fill_manual(values = colors) +
  ylab("Nombre de candidats avec faible activité Twitter\n") +
  xlab("\nDifférence avec le parti gagnant dans la circonscription en 2019 (log)") +
  facet_wrap(facets = ~Parti, scales = "free_y") +
  theme_bw(base_size = 7.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggsave("_SharedFolder_RecherchePoteaux/vote2019/aveczeros.png")
                                
ggplot(merge, aes(x = n_tw)) +
  geom_histogram(binwidth = 5) +
  coord_cartesian(xlim = c(0, 1000))


#Différence avec le deuxième 

difdeux <- merge %>%
  group_by(IdCirc) %>%
  top_n(2, NbVotes) %>%
  mutate(PCTR = min(PCT), 
        diff = PCT - PCTR ) %>%
  filter(diff > 0)


# Vainqueur 

ggplot(difdeux, aes(x = diff, y = log(n_tw))) +
  geom_point(aes(color = Parti), size = 1) +
  geom_smooth(se = F, size = 1) +
  scale_color_manual(values = colors) +
  ylab("Nombre de tweets du candidat pendant\nla campagne électorale de 2021 (log)\n") +
  xlab("\n PCT d'écart avec le deuxième (log)") +
  facet_wrap(facets = ~Parti, scales = "free") +
  theme_bw(base_size = 7.5) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggsave("_SharedFolder_RecherchePoteaux/vote2019/vainqueurs.png")


