
Data <- readRDS("_SharedFolder_RecherchePoteaux/vote2019/ResultatsParBureau2019.rds")
parties <- c("NDP" = "NPD", "CPC" = "PCC", "LPC" = "PLC",
             "BQ" = "BQ", "PPC" = "PPC", "GPC" = "PVC")
provinces <- c("10" = "tnl", "11" = "ipe", "12" = "ns", "13" = "nb",
               "24" = "qc", "35" = "on", "46" = "mb", "47" = "sk",
               "48" = "ab", "59" = "bc", "60" = "yk", "61" = "nw", "62" = "nu")
circs_MTL <- c(24003, 24015, 24028, 24029, 24037,
               24036, 24033, 24024, 24039, 24052,
               24053, 24054, 24055, 24056, 24064,
               24068, 24069, 24077)
circs_QCcity <- c(24020, 24008, 24019, 24044, 24045, 24058, 24059)
circs_TOR <- c(35027, 35028, 35029, 35081, 35120, 35121,
               35118, 35024, 35018, 35090, 35110, 35101,
               35109, 35108, 35019, 35020, 35021, 35007,
               35093, 35094, 35095, 35096, 35097, 35098, 35115)
TwitterData <- readRDS("_SharedFolder_RecherchePoteaux/vote2019/twitter.rds") %>% 
  mutate(party = parties[data.currentParty]) %>% 
  select(-data.currentParty)
names(TwitterData) <- c("id_circ", "n_tweets", "has_twitter", "party")

ByCirc <- Data %>% 
  group_by(IdCirc, Parti) %>% 
  summarise(votes = sum(NbVotes)) %>% 
  mutate(votes_circ = sum(votes),
         prop = (votes/votes_circ)*100,
         rri1 = prop-max(prop))

RRI <- ByCirc %>% 
  filter(rri1 != 0) %>% 
  group_by(IdCirc) %>% 
  mutate(max = max(prop)) %>% 
  filter(prop == max) %>% 
  select(IdCirc, second = prop) %>% 
  right_join(., ByCirc, by = "IdCirc") %>% 
  mutate(rri = ifelse(rri1 == 0, prop-second, rri1)) %>% 
  select(id_circ = IdCirc, party = Parti, rri) %>% 
  left_join(., TwitterData, by = c("id_circ", "party")) %>% 
  replace_na(list(n_tweets = 0, has_twitter = 0)) %>% 
  filter(!(party %in% c("PPC", "autre"))) %>% 
  mutate(prov = provinces[substr(id_circ, 1, 2)],
         qcCity = ifelse(id_circ %in% circs_QCcity, 1, 0),
         mtl = ifelse(id_circ %in% circs_MTL, 1, 0),
         tor = ifelse(id_circ %in% circs_TOR, 1, 0),
         has_twitter = ifelse(has_twitter == 1, "With Twitter",
                              "Without Twitter")) %>% 
  fastDummies::dummy_cols(., select_columns = "prov") %>% 
  fastDummies::dummy_cols(., select_columns = "party")

library(ggridges)
ggplot(RRI, aes(x = rri, y = factor(has_twitter,
                                    levels = c("Without Twitter",
                                               "With Twitter")),
                color = has_twitter, fill = has_twitter)) +
  geom_density_ridges(bandwidth = 1.5,
                      scale = 0.95,
                      alpha = 0.75,
                      show.legend = F) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c("#1DA1F2", "#AAB8C2")) +
  scale_fill_manual(values =  c("#1DA1F2", "#AAB8C2")) +
  theme_ridges() +
  xlab("Party's RRI in the candidate's\nriding in 2019") +
  ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 12, hjust = 0.5))
ggsave("_SharedFolder_RecherchePoteaux/graphs/account_vs_noacc.png",
       width = 7, height = 5)  

ggplot(RRI, aes(x = rri, y = factor(has_twitter,
                                    levels = c("Without Twitter",
                                               "With Twitter")),
                color = has_twitter, fill = has_twitter)) +
  geom_density_ridges(bandwidth = 3,
                      scale = 0.95,
                      alpha = 0.75,
                      show.legend = F) +
  facet_wrap(~party) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c("#1DA1F2", "#AAB8C2")) +
  scale_fill_manual(values =  c("#1DA1F2", "#AAB8C2")) +
  theme_ridges() +
  xlab("Party's RRI in the candidate's\nriding in 2019") +
  ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 12, hjust = 0.5))
ggsave("_SharedFolder_RecherchePoteaux/graphs/account_vs_noacc_party.png",
       width = 7, height = 5)  

RRI$rri_norm <- normalize(RRI$rri)
RRI$ntweets_norm <- normalize(RRI$n_tweets)

ModelData <- RRI %>% 
  select(ntweets_norm, rri_norm, has_twitter, qcCity, mtl, tor, starts_with("prov"), starts_with("party"), -prov, -party)

# shitty plot
ggplot(RRI, aes(x = rri, y = log(n_tweets))) +
  geom_point() +
  geom_smooth() +
  facet_grid(rows = vars(prov), cols = vars(party))


# Regressions ####
Model <- regress("ModelData", names(ModelData)[1], names(ModelData)[-1], reg_type = "lm")
summary(Model)

ModelQC <- ModelData %>% 
  filter(prov_qc == 1) %>% 
  select(ntweets_norm, rri_norm, qcCity, mtl,
         party_BQ, party_NPD, party_PCC, party_PLC, party_PVC)
ModelQc <- regress("ModelQC", names(ModelQC)[1], names(ModelQC)[-1], reg_type = "lm")
summary(ModelQc)

model <- lm(ntweets_norm ~ rri_norm + qcCity*party_BQ + qcCity*party_PCC +
                          qcCity*party_PLC + qcCity*party_NPD +
                          mtl*party_BQ + mtl*party_PCC +
                          mtl*party_PLC + mtl*party_NPD,
   data = ModelQC)
summary(model)

