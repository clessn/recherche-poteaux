library(tidyverse)

Data <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/tweets.rds")

Persons <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/persons.rds") %>% 
  select(data.twitterHandle, data.currentDistrict, data.currentParty, data.currentProvinceOrState,
         data.firstName, data.lastName, data.fullName, data.isFemale, data.twitterAccountCreatedOn) %>% 
  filter(data.currentParty != "PPC")

Agg <- Data %>% 
  group_by(metadata.twitterHandle, type) %>% 
  summarise(n = n())


