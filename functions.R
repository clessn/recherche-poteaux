library(tidyverse)
library(haven)
library(data.table)
library(cowplot)
library(ggtext)

normalize <- function(vector){
  min <- min(vector)
  max <- max(vector)
  vec <- (vector-min)/(max-min)
  return(vec)
}

regress <-
  function(data,
           y,
           x,
           reg_type,
           controls = as.character(),
           family = NA) {
    x_express <- paste0(x, collapse = " + ")
    vars_express <- paste0(y, " ~ ", x_express)
    controls_express <- paste0(controls, collapse = " + ")
    varsWcont_express <- if (length(controls) == 0) {
      vars_express
    }
    else {
      paste0(vars_express, " + ", controls_express)
    }
    express <-
      paste0(reg_type, "(", varsWcont_express, ", data = ", data)
    express <- if (is.na(family)) {
      paste0(express, ")")
    }
    else {
      paste0(express, ", family = ", family, ")")
    }
    model <- eval(parse(text = express))
    return(model)
  }

# df with 3 columns:
###  id_circ
###  party
###  vote_share
generate_rri <- function(df) {
  print("The function will only work if the variables in `df` are in this order:")
  print("    1. id_riding")
  print("    2. party")
  print("    3. vote_share")
  names(df) <- c("id_riding", "party", "vote_share")
  step_one <- df %>% 
    group_by(id_riding) %>% 
    mutate(rri1 = vote_share-max(vote_share))
  RRI <- step_one %>% 
    filter(rri1 != 0) %>% 
    group_by(id_riding) %>% 
    mutate(max = max(vote_share)) %>% 
    filter(vote_share == max) %>% 
    select(id_riding, second = vote_share) %>% 
    right_join(., step_one, by = "id_riding") %>% 
    mutate(rri = ifelse(rri1 == 0, vote_share-second, rri1)) %>% 
    select(id_riding, party, rri)
  return(RRI)
}
