library(tidyverse)

Data <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/twitter.rds") %>%
  mutate(party = parties[data.currentParty]) %>% 
  select(-data.currentParty)

#candidates <- table_candidates

Duplicates <- Data %>% 
  distinct() %>% 
  group_by(ED_CODE, party) %>% 
  summarise(n = n()) %>% 
  filter(n == 2) %>% 
  mutate(id_riding_party = paste0(ED_CODE, party))


id_nom <- readRDS("_SharedFolder_RecherchePoteaux/vote2021/ResultatsParCirc2021.rds") %>% 
  mutate(IdCirc = as.integer(IdCirc)) %>% 
  select(IdCirc, NomCirc_FR)

Duplicates <- left_join(Duplicates, id_nom, by = c("ED_CODE" = "IdCirc")) %>% 
  distinct() %>% 
  left_join(., table_candidates, by = c("NomCirc_FR" = "data.currentDistrict"))


Check <- Data %>%
  mutate(id_riding_party = paste0(ED_CODE, party)) %>% 
  filter(id_riding_party %in% Duplicates$id_riding_party &
           party != "PPC") %>% 
  left_join(., id_nom, by = c("ED_CODE" = "IdCirc")) %>% 
  distinct() %>% 
  select(NomCirc_FR, ED_CODE, party, n_tw, has_twi)


Validation <- readxl::read_xlsx("_SharedFolder_RecherchePoteaux/validate_problematic-hubert.xlsx") %>% 
  filter(is.na(Validé))

NewData <- Validation %>% 
  select(ED_CODE, n_tw = `Nb tweets`, has_twi = `Compte Twitter`, party)

Check$id_riding_party <- paste0(Check$ED_CODE, Check$party)
Data$id_riding_party <- paste0(Data$ED_CODE, Data$party)

Data <- Data %>% 
  filter(!(id_riding_party %in% Check$id_riding_party)) %>% 
  select(-id_riding_party) %>% 
  rbind(., NewData) %>% 
  distinct()

saveRDS(Data, "_SharedFolder_RecherchePoteaux/ready_data/twitter2.rds")

Validate <- Data %>% 
  filter(party != "PPC") %>% 
  group_by(ED_CODE, party) %>% 
  summarise(n = n())
