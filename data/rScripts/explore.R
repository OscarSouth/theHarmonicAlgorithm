library("tidyverse")
setwd("/home/oscarsouth/haskellProjects/theHarmonicAlgorithm/data/rScripts")
bach <- read_csv("../jsbach_chorals_harmony.data", 
                 col_names = c(
                   "seq", "event",
                   "0", "1", "2", "3", "4", "5", 
                   "6", "7", "8", "9", "10", "11",
                   "fund", "acc", "label"
                 ), cols(
                   seq = col_character(),
                   event = col_integer(),
                   `0` = col_character(),
                   `1` = col_character(),
                   `2` = col_character(),
                   `3` = col_character(),
                   `4` = col_character(),
                   `5` = col_character(),
                   `6` = col_character(),
                   `7` = col_character(),
                   `8` = col_character(),
                   `9` = col_character(),
                   `10` = col_character(),
                   `11` = col_character(),
                   fund = col_character(),
                   acc = col_integer(),
                   label = col_character()
                 )
               )

bach <-
  bach %>% 
    select(seq, event, fund, acc) %>%
    add_column(pitch = bach %>% 
                 select(`0`:`11`) %>% 
                 t() %>% 
                 as.data.frame() %>%
                 unname() %>%
                 map(function(x) str_which(x, "YES")-1)
              )