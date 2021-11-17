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

# Merge vote et twitter ####
merge <- left_join(twitter, vote, by = c("IdCirc", "Parti"))
  
ggplot(merge, aes(x = PCT, y = ))

                                
                                
 