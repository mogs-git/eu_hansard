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

# Analysis begins with extracting names in order from text.
name_vec <- extract_names(eu)

# Then get parties by looking at text structure and webscraping
appearances_tib <- get_parties(name_vec)

appearances_tib %<>% add_surname()

# Manually fix Lords missing parties ----
appearances_tib %>% filter(is.na(party)) %>% unique()

# Add more variables ----

appearances_tib %<>% add_gender()

# real deal hocus pocus step
appearances_tib %<>% bind_speeches()

full_speeches <- reduce_speeches(appearances_tib)

# Reduce party data
appearances_tib %<>% keep_main_parties()
full_speeches %<>% keep_main_parties()

saveRDS(appearances_tib, "data//appearances.RDAT")
saveRDS(speech_tib, "data//full_speeches.RDAT")

party_id_tib <- distinct(full_speeches, name, party)
(party_id_tib %<>% keep_main_parties() %>% add_surname)
saveRDS(party_id_tib, "data//party_id_tib.RDAT")
