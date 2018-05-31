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
tib <- tibble(name = names,
              containing_party = str_detect(names, "\\(")) 

tib_party <- filter(tib, containing_party == TRUE)
tib_party <- tibble(
  name = str_extract(tib_party$name, "[a-zA-Z ',\\-]+"),
  party = str_extract(tib_party$name, "\\([^()]+\\)")
)
tib_party <- unique(tib_party)
tib$name <- str_replace(tib$name, "\\(.+\\)", "")
tib$name <- trimws(tib$name)
tib_party$name <- trimws(tib_party$name)

ID_tib <- left_join(tib, tib_party) %>% select(name, party)
surname_join <- tibble(name = unique(ID_tib$name), surname = get_surnames(unique(ID_tib$name)))
appearances_tib <- get_parties(names)

# For missing lords, rather than play around with text, just web scrape. 

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


main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
appearances_tib %<>% mutate(r = row_number(), party_f = ifelse(party %!in% main_parties, "other", party)) 
appearances_tib$party_f <- fct_relevel(appearances_tib$party_f, c("(Con)", "(CB)", "(LD)", "(Lab)", "other"))
appearances_tib$party_f %>% levels()
# appearances_tib$party_f %<>% fct_rev()


saveRDS(appearances_tib, "data//appearances.RDAT")
saveRDS(speech_tib, "data//full_speeches.RDAT")


get_surnames <- function(names) {
  # take a vector of names + epiphets, and extract second word (surname)
  pulled_names <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+[-']?\\w+)(\\W\\w+(?!of))?") 
  
  # If name is double barelled but has no "-", e.g. De Mauley, then concatenate
  for(i in seq_along(pulled_names[,3])) {
    if (!str_detect(pulled_names[,3][i], "of|and") & !is.na(pulled_names[,3][i])) {
      pulled_names[,2][i] <- str_c(pulled_names[,2][i], "_", str_extract(pulled_names[,3][i], "\\w+"))
    }
  }
  surname <- pulled_names[,2]
  
  # Find duplicates
  dups <- get_duplicates(surname) %>% keep(!is.na(.))
  replacements <- list()
  for (i in seq_along(dups)) {
    indexes <- dups[[i]]
    for (j in seq_along(indexes)) {
      fulnam <- str_match_all(names[[indexes[[j]]]], "\\w+") %>% unlist()
      if (length(fulnam) >= 4) {
        replacements <- list_push(replacements, c(str_c(fulnam[2], "_", fulnam[4]), indexes[j]))
      }
    }
  }
  replacements <- replacements[!duplicated(replacements)]
  for (i in seq_along(replacements)) {
    surname[[as.numeric(replacements[[i]][2])]] <- replacements[[i]][1]
  }
  surname
}

get_duplicates <- function(vec) {
  dups <- list()
  for (i in seq_along(vec)) {
    dups[[i]] <- which(vec == vec[i])
    if(length(dups[[i]]) == 1) {
      dups[[i]] <- NA
    }
    names(dups)[[i]] <- vec[i]
  }
  dups
}

surname <- get_surnames(speech_tib$name) 
surname_join <- tibble(name = speech_tib$name, surname = surname)
party_id_tib <- distinct(full_speeches, name, party)
(party_id_tib %<>% keep_main_parties() %>% left_join(surname_join))
saveRDS(party_id_tib, "data//party_id_tib.RDAT")
