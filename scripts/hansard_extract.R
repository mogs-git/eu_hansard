# Initial extraction and munging

# Inputs: Hansard source text, source functions 
eu <- read_file("data/European Union (Notification of Withdrawal) Bill 2017-03-01 (1).txt")
source('scripts\\main_src.R')
# Outputs: 
# Appearances_tib: speeches in order with Lord, party, gender etc...
# Full_speeches: concatenated speeches of each lord. 

# packages ----
pacman::p_load(tidyverse, stringr, forcats, purrr, magrittr, tidytext, reshape2, wordcloud)

# extract ----

names <- extract_names(eu)
appearances_tib <- get_parties(names)

# Manually fix Lords missing parties ----
appearances_tib %>% filter(is.na(party)) %>% unique()

# The Archbishop and Baroness are not assigned a party. 
str_subset(names, "Headley|Elie|York|Scone") %>% unique()

for (i in seq_along(appearances_tib$name)) {
  if (appearances_tib$name[i] == "Lord Bridges of Headley"){
    appearances_tib$party[i] = "(Con)"
  } else if (appearances_tib$name[i] == "Lord Keen of Elie") {
    appearances_tib$party[i] = "(Con)"
  } else if (str_detect(appearances_tib$name[i], "Advocate-General")) {
    appearances_tib$name[i] = "Lord Bridges of Headley"
  } else if (str_detect(appearances_tib$name[i], "Parliamentary Under-Secretary")) {
    appearances_tib$name[i] = "Lord Keen of Elie"
  }
}

# Add more variables ----

appearances_tib %<>% add_gender()

appearances_tib %<>% bind_speeches()

speech_tib <- reduce_speeches(appearances_tib)

# Reduce party data

keep_main_parties <- function(df) {
  main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
  df %<>% mutate(party_f = ifelse(party %!in% main_parties, "other", party)) 
  df$party_f <- fct_relevel(df$party_f, c("(Con)", "(CB)", "(LD)", "(Lab)", "other"))
  df
}
main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
appearances_tib %<>% mutate(r = row_number(), party_f = ifelse(party %!in% main_parties, "other", party)) 
appearances_tib$party_f <- fct_relevel(appearances_tib$party_f, c("(Con)", "(CB)", "(LD)", "(Lab)", "other"))
appearances_tib$party_f %>% levels()
# appearances_tib$party_f %<>% fct_rev()


saveRDS(appearances_tib, "data//appearances.RDAT")
saveRDS(speech_tib, "data//full_speeches.RDAT")


party_id_tib <- distinct(full_speeches, name, party)
(party_id_tib %<>% keep_main_parties())
saveRDS(party_id_tib, "data//party_id_tib.RDAT")
