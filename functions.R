library(tidyverse)

generate_rri <- function(party, ){
    ncols <- length(names(prepped_data))
    StepOne <- prepped_data %>%
      pivot_longer(., all_of(2:ncols),
                   names_to = "party",
                   values_to = "potGrowth")
    Max <- StepOne %>%
      group_by(!!sym(id_col)) %>%
      summarise(first = max(potGrowth))
    FirstSecond <- left_join(StepOne, Max, by = id_col) %>%
      mutate(ismax = ifelse(potGrowth == first, 1, 0)) %>%
      group_by(!!sym(id_col)) %>%
      mutate(nismax = sum(ismax)) %>%
      filter(potGrowth != first) %>%
      group_by(!!sym(id_col)) %>%
      mutate(second = ifelse(nismax > 1, first, max(potGrowth))) %>%
      group_by(!!sym(id_col)) %>%
      summarise(first = unique(first),
                second = unique(second)) %>%
      select(id_col, first, second)
    ThData <- left_join(StepOne, FirstSecond, by = id_col) %>%
      mutate(irc = ifelse(potGrowth == first, potGrowth - second,
                          potGrowth - first)) %>%
      pivot_wider(., id_cols = !!sym(id_col), 
                  names_from = party,
                  values_from = irc)
    if (!(is.na(former_prefix))){
      names(ThData) <- gsub(former_prefix, new_prefix, names(ThData))
    } else {
      names(ThData) <- names(ThData)
    }
    return(ThData)
  }
  
}