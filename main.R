# 0. Packages and functions ####
source("functions.R", encoding = "UTF-8")

# 1. Define useful variables ####
party_names_plot <- c(
  PLC = "LPC",
  PCC = "CPC",
  NPD = "NDP",
  BQ = "BQ",
  PVC = "GPC"
)
party_colors <- data.frame(var = c("LPC", "CPC", "NDP", "BQ", "GPC"),
                          var_color= c("#D71920","#1A4782","#F37021","#33B2CC", "#3D9B35"))
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
# 2. Data ####
## 2.1 Load Data ####
Vote19 <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/voteresults2019.RDS")
Vote21 <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/voteresults2021.RDS")
Twitter <- readRDS("_SharedFolder_RecherchePoteaux/ready_data/twitter.rds") %>%
  mutate(party = parties[data.currentParty]) %>% 
  select(-data.currentParty)
names(Twitter) <- c("id_riding", "n_tweets", "has_twitter", "party")

## 2.2 Bind RRI dataframes for 2019 and 2021 ####
rri19 <- generate_rri(Vote19)
rri21 <- generate_rri(Vote21) %>% 
  mutate(id_riding = as.numeric(id_riding))

## 2.3 join Twitter Data
data19 <- rri19 %>% 
  left_join(., Twitter, by = c("id_riding", "party")) %>% 
  replace_na(list(n_tweets = 0, has_twitter = 0)) %>% 
  filter(!(party %in% c("PPC", "autre"))) %>% 
  mutate(prov = provinces[substr(id_riding, 1, 2)],
         qcCity = ifelse(id_riding %in% circs_QCcity, 1, 0),
         mtl = ifelse(id_riding %in% circs_MTL, 1, 0),
         tor = ifelse(id_riding %in% circs_TOR, 1, 0),
         has_twitter = ifelse(has_twitter == 1, "With Twitter",
                              "Without Twitter")) %>% 
  fastDummies::dummy_cols(., select_columns = "prov") %>% 
  fastDummies::dummy_cols(., select_columns = "party")

data21 <- rri21 %>% 
  left_join(., Twitter, by = c("id_riding", "party")) %>% 
  replace_na(list(n_tweets = 0, has_twitter = 0)) %>% 
  filter(!(party %in% c("PPC", "autre"))) %>% 
  mutate(prov = provinces[substr(id_riding, 1, 2)],
         qcCity = ifelse(id_riding %in% circs_QCcity, 1, 0),
         mtl = ifelse(id_riding %in% circs_MTL, 1, 0),
         tor = ifelse(id_riding %in% circs_TOR, 1, 0),
         has_twitter = ifelse(has_twitter == 1, "With Twitter",
                              "Without Twitter")) %>% 
  fastDummies::dummy_cols(., select_columns = "prov") %>% 
  fastDummies::dummy_cols(., select_columns = "party")

# 3. Graphs ####

## additional dfs ####
WinLoseByParty21 <- data21 %>%
  mutate(win = ifelse(rri > 0, 1, 0)) %>% 
  group_by(party, has_twitter, win) %>% 
  summarise(n = n()) %>% 
  mutate(n_group = sum(n),
         prop = (n/n_group)*100)
  
WinLose21 <- WinLoseByParty21 %>%
  group_by(has_twitter, win) %>% 
  summarise(n = sum(n)) %>% 
  mutate(n_group = sum(n),
         prop = (n/n_group)*100) %>% 
  ungroup() %>% 
  mutate(x = c(-48, 36.5, -55, 20),
         y = c(2.35, 2.28, 1.57, 1.15),
         label = paste0(round(prop), "%"))

## 2019 rri ####

### all parties ####
ggplot(data19, aes(x = rri, y = factor(has_twitter,
                                    levels = c("Without Twitter",
                                               "With Twitter")),
                color = has_twitter, fill = has_twitter)) +
  geom_density_ridges(bandwidth = 1.5,
                      scale = 0.95,
                      alpha = 0.75,
                      show.legend = F,
                      panel_scaling = T) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c("#1DA1F2", "#AAB8C2")) +
  scale_fill_manual(values =  c("#1DA1F2", "#AAB8C2")) +
  geom_text(data = WinLose19,
            aes(x = x,
                y = y,
                label = label),
            size = 8,
            #color = "black",
            #fontface = "bold",
            show.legend = F) +
  theme_ridges() +
  xlab("Party's RRI in the Candidate's\nRiding in 2019") +
  ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 12, hjust = 0.5)) +
  geom_vline(xintercept = 0,
             size= 1.5)
ggsave("_SharedFolder_RecherchePoteaux/graphs/account_vs_noacc19.png",
       width = 10, height = 7)  

### by parties ####
ggplot(data19, aes(x = rri, y = factor(has_twitter,
                                       levels = c("Without Twitter",
                                                  "With Twitter")),
                   color = has_twitter, fill = has_twitter)) +
  geom_density_ridges(bandwidth = 3,
                      scale = 0.95,
                      alpha = 0.75,
                      show.legend = F) +
  facet_wrap(~party,
             labeller = as_labeller(party_names_plot)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c("#1DA1F2", "#AAB8C2")) +
  scale_fill_manual(values =  c("#1DA1F2", "#AAB8C2")) +
  theme_ridges() +
  xlab("Party's RRI in the candidate's\nriding in 2019") +
  ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        strip.background = element_blank(),
        strip.text = element_textbox(
          size = 12,
          color = "#F5F8FA", fill = "#14171A", box.color = "#14171A",
          halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(0.9, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
        )) +
  geom_vline(xintercept = 0,
             size= 1)
ggsave("_SharedFolder_RecherchePoteaux/graphs/account_vs_noacc_party19.png",
       width = 10, height = 7)

## 2021 rri ####

### all parties ####
ggplot(data21, aes(x = rri, y = factor(has_twitter,
                                       levels = c("Without Twitter",
                                                  "With Twitter")),
                   color = has_twitter, fill = has_twitter)) +
  geom_density_ridges(bandwidth = 1.5,
                      scale = 0.95,
                      alpha = 0.75,
                      show.legend = F,
                      panel_scaling = T) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c("#1DA1F2", "#AAB8C2")) +
  scale_fill_manual(values =  c("#1DA1F2", "#AAB8C2")) +
  geom_text(data = WinLose21,
            aes(x = x,
                y = y,
                label = label),
            size = 8,
            #fontface = "bold",
            show.legend = F) +
  theme_ridges() +
  xlab("Party's RRI in the Candidate's\nRiding in 2021") +
  ylab("") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.x = element_text(size = 12, hjust = 0.5)) +
  geom_vline(xintercept = 0,
             size= 1.5)
ggsave("_SharedFolder_RecherchePoteaux/graphs/account_vs_noacc21.png",
       width = 10, height = 7)  


