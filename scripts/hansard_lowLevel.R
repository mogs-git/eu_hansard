# load----
pacman::p_load(tidyverse, magrittr, tidytext, stringr)

full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")
`%!in%` <- negate(`%in%`)

tidy_dtm <- appearances_tib %>% 
  mutate(r = row_number()) %>% 
  unnest_tokens(word, speeches)

# Number of questions

full_speeches %>% group_by(name) %>% mutate(numQs = str_count(all_speeches, "\\?")) %>% View()

s <- full_speeches %>% filter(name == "Lord Oates") %>% pull(all_speeches)
qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Whas|\\Wwill|\\?)"
str_extract_all(s, "\\w+\\?")
str_extract_all(s, "\\?")
initial_matches <- str_extract_all(s, qwords)[[1]]
initial_match_indexes_start <- which(initial_matches == "?") - 1
initial_match_indexes_end <- which(initial_matches == "?")
qword_positions <- str_locate_all(s, qwords)[[1]]
str_sub(s, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])

# Pull questions
pull_questions <- function(string) {
  # pull the questions asked in a string, and the question word used.
  qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Wis|\\Whas|\\Wwill|\\Wdoes|\\?)"
  initial_matches <- str_extract_all(string, qwords)[[1]]
  initial_match_indexes_start <- which(initial_matches == "?") - 1
  initial_match_indexes_end <- which(initial_matches == "?")
  qword_positions <- str_locate_all(string, qwords)[[1]]
  question_sentence <- str_sub(string, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])
  question_word <- initial_matches[initial_match_indexes_start]
  
  sentence_matches <- str_extract_all(string, "\\.|\\:|\\?")[[1]]
  sentence_matches_start <- which(sentence_matches == "?") - 1
  sentence_matches_end <- which(sentence_matches == "?")
  qsentence_positions <- str_locate_all(string, "\\.|\\:|\\?")[[1]]
  question_sentence_full <- str_sub(string, qsentence_positions[sentence_matches_start], qsentence_positions[sentence_matches_end])
  
  return(tibble(question_word, question_sentence, question_sentence_full))
}

pull_questions(s) %>% View()

all_questions <- full_speeches %>%
  group_by(name) %>% 
  do(pull_questions(str_to_lower(.$all_speeches)))

# Number of unique words used

total_words <- unique(tidy_dtm$word)
tidy_dtm %>% 
  group_by(word) %>% mutate(total_count = n()) %>%
  group_by(name, word) %>% mutate(speaker_count = n()) %>%
  group_by(name) %>% summarise(total_words = sum(speaker_count), num_unique = sum(total_count == speaker_count), prop_unique = num_unique/total_words) %>%
  arrange(desc(prop_unique))

# Number of times other lords are mentioned

surname <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+)") 
surname <- surname[,2] %>% str_to_lower()
for (i in seq_along(surname)) {
  surname[i] <- str_c("\\s", surname[i], "\\W")
}

surname <- surname[which(surname != '\\sO\\W' & surname != '\\sDe\\W')]
pronouns <- c('his lordship', 'the baroness', 'the lord', 'the minister', 'her ladyship', 'the noble lords?(?!(.{0,4}lord))', 'reverend primate')
named_references <- c(surname, pronouns) 
named_references_reg <- str_c("(", str_c(named_references, collapse = "|"), ")")

# Problems: Some names match other words e.g. 'true', 'hunt'. double barrelled names. 
# instances where someone refers to 'the noble lord, lord blencathra' will be counted twice
# could solve this using negative lookahead on 'the noble'
appearances_tib %<>% mutate(speeches = str_to_lower(speeches)) %>% filter(!is.na(speeches))

appearances_tib %>% group_by(name) %>% mutate(n_references = str_count(speeches, named_references_reg)) %>% View()

s <- appearances_tib %>% filter(name == 'Lord Winston') %>% pull(speeches) 
s <- appearances_tib$speeches[[1]] 
s <- "The noble Lord, Lord Howard, and his noble steed carrying him, are followed by Lord Chubbington and his noble donkey, Assworth. There was supposedly a third lord, but the noble Lord being too drunk, did not arrive for the adventure."
str_view_all(s, named_references_reg)
str_extract_all(s, named_references_reg)

# could also use entity extractor to get references to people, but
# requires capitalised names and locations. 
library(NLP)
library(openNLP)
library(openNLPmodels.en)
string_s <- as.String(s)
mea <- Maxent_Entity_Annotator(kind = "person")
sent_token_annotator <-Maxent_Sent_Token_Annotator()
word_token_annotator <-Maxent_Word_Token_Annotator()
a2 <-annotate(string_s, list(sent_token_annotator, word_token_annotator, mea))
a2
k <- sapply(a2$features, `[[`, "kind")
map(a2$features, `[[`, "kind")
string_s[a2[k == "person"]]
# want to find number of qs per x many words

# Words in context ----

wordInContext_rgx <- function(txt, word) {
  # lower case full string vector (txt) and search term (word).
  txt %>% str_extract_all(str_c("(^\\W*|\\w+\\W+)", word, "s?(\\W+\\w+|\\W*$)"))
}

full_speeches %<>% mutate(all_speeches = map(all_speeches, str_to_lower))

full_speeches$all_speeches[[1]] %>% wordInContext_rgx("eu")
full_speeches$all_speeches[[1]] %>% wordInContext_rgx("rights")

# 
