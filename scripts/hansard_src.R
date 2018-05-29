# Hansard analysis source file

# Extract ----

# Input: dataframe containing a party variable 
# What: keeps Con, Lab, Crossbench or Libdem, replacing other parties with 'other'
# Returns: dataframe
keep_main_parties <- function(df) {
  main_parties <- c("(Con)", "(Lab)", "(CB)", "(LD)")
  df %<>% mutate(party_f = ifelse(party %!in% main_parties, "other", party)) 
  df$party_f <- fct_relevel(df$party_f, c("(Con)", "(CB)", "(LD)", "(Lab)", "other"))
  df
}

# Input: list and value to be appended
# What: Appends an item to a list (equivalent to l.append() in python)
# Returns: list with new value appended to end of it
list_push <- function(l, a) {
  l[[length(l) + 1]] <- a 
  return(l)
}

# Input: vector of lord's names
# What: Extract the surname
# Returns: A vector of surnames
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

# Input: A vector of things containing duplicated values
# What: finds the name and position of duplicates 
# Returns: A vector of format c(c(duplicate_name, duplicate_index), ...)
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

# Input: dataframe containing a name variable (not surname)
# What: Interprets gender from title e.g. Lord = male
# Returns: dataframe with a gender variable

add_gender <- function(df = appearances_tib) {
  gender_vector <- vector("character", length(df$name))
  for (i in seq_along(df$name)) {
    if (str_detect(df$name[i], "Lord|Viscount|Archbishop|Keen|Bridges")) {
      gender_vector[i] <- "male"
    } else {
      gender_vector[i] <- "female"
    }
  }
  
  df %<>% add_column(gender = gender_vector)
}

# Counts ----

# Order ----

# LowLevel ----
# Input: a string of text data
# What: extract the questions asked in the string.
# Returns: dataframe containing the last question word used, the immediate sentence from
# the last q word, and the full sentence.
pull_questions <- function(string) {
  # pull the questions asked in a string, and the question word used.
  qwords <- "(\\Wdo|\\Wwhat|\\Wwhere|\\Wwho|\\Wwhy|\\Wwhen|\\Whow|\\Wif|\\Wis|\\Whas|\\Wwill|\\Wdoes|\\?)"
  
  # Get the position of every question word and question mark
  initial_matches <- str_extract_all(string, qwords)[[1]]
  
  # Get the index of each question mark and the question word preceding it.
  initial_match_indexes_start <- which(initial_matches == "?") - 1
  initial_match_indexes_end <- which(initial_matches == "?")
  
  # Pull out the words between the question words and the question marks.
  qword_positions <- str_locate_all(string, qwords)[[1]]
  question_sentence <- str_sub(string, qword_positions[initial_match_indexes_start], qword_positions[initial_match_indexes_end])
  question_word <- initial_matches[initial_match_indexes_start]
  
  # Pull out the words between the end of the last sentence prior to each question mark.
  sentence_matches <- str_extract_all(string, "\\.|\\:|\\?")[[1]]
  sentence_matches_start <- which(sentence_matches == "?") - 1
  sentence_matches_end <- which(sentence_matches == "?")
  qsentence_positions <- str_locate_all(string, "\\.|\\:|\\?")[[1]]
  question_sentence_full <- str_sub(string, qsentence_positions[sentence_matches_start], qsentence_positions[sentence_matches_end])
  
  return(tibble(question_word, question_sentence, question_sentence_full))
}

# Input: a string and a phrase 
# What: Extract the words adjacent to the word.
# Returns: A vector of "words in context"
wordInContext_rgx <- function(txt, word) {
  # lower case full string vector (txt) and search term (word).
  txt %>% str_extract_all(str_c("(^\\W*|\\w+\\W+)", word, "s?(\\W+\\w+|\\W*$)"))
}

# Input: a string and a phrase
# What: Extract sentence including phrase
# Returns: sentences including the phrase
wic_sentence <- function(txt, word) {
  endpuncs <- "(\\?|\\.|\\!)"
  txt %>% str_extract_all(str_c("(\\.|\\!|\\?)[^.!?]+\\W", word, "(\\.|\\W[^.!?]*?(\\.|\\!|\\?))")) %>% `[[`(1)
}

# Input: type of grammar 
# What: Part-of-speech tagger generator function
# Returns: A function that can be used to tag and extract
# specific grammar types from a string.
anno_ <- function(p = "NN") {
  build_annos <- map_lgl(c("sent_token_annotator", "word_token_annotator", "pos_token_annotator"), exists)
  if(sum(build_annos != 3)) {  
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  }
  
  anno <- function(otxt, pat = p) {
    txt <- as.String(otxt)
    a2 <- annotate(txt, list(sent_token_annotator, word_token_annotator))
    a3 <- annotate(txt, pos_tag_annotator, a2)
    #a3probs <- annotate(txt, Maxent_POS_Tag_Annotator(probs = TRUE), a2)
    #a3pw <- subset(a3probs, type == "word")
    #probs <- sapply(a3pw$features, `[[`, "POS_prob")
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    tags_f <- tags[str_detect(txt[a3w], "[A-Za-z]")]
    txt_tib <- tibble(word = txt[a3w], tags)
    txt_tib %>%
      filter(str_detect(tags, pat)) %>% left_join(pos_tag_abbrs, by = c("tags" = "abbreviation")) %>% filter(!str_detect(word, "\\W"))
  } 
}
