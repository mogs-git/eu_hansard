# Hansard analysis source file

# Extract ----

# All the times a name appears in order of appearance
extract_names <- function(text, prefix_string = "\r\n\r\n((Lord|The Advocate|The Earl|The Countess|The Archbishop|The Parliamentary|Viscount|Baroness|^Noble Lords$)(.+))(\r\n)") {
  # prefix_string = "\r\n\r\n((Lord|The Advocate|The Countess|The Archbishop|The Parliamentary|Viscount|Baroness|^Noble Lords$)(.+))(\r\n)"
  names <- text %>%
    str_extract_all(prefix_string) %>%
    unlist() %>%
    str_sub(start = 5, end = -3) # Remove leading and trailing escape characters
  
  names
}


# Add gender based on title
add_gender <- function(df = appearances_tib) {
  gender_vector <- vector("character", length(df$name))
  for (i in seq_along(df$name)) {
    if (str_detect(df$name[i], "Lord|Viscount|Archbishop|Keen|Bridges|Earl")) {
      gender_vector[i] <- "male"
    } else {
      gender_vector[i] <- "female"
    }
  }
  
  df %<>% add_column(gender = gender_vector)
}

# Extract the speech made by each lord in chronological order
bind_speeches <- function(df = appearances_tib, text = eu, rm_shouts = F) {
  
  names_search <- text %>% str_extract_all("\r\n\r\n((Lord|The Advocate|The Countess|The Earl|The Archbishop|The Parliamentary|Viscount|Baroness|^Noble Lords$)(.+))(\r\n)") %>%
    unlist() 
  names_search_u <- unique(names_search)
  ns <- str_c(names_search_u, collapse = "|")
  ns <- str_replace_all(ns, "\\(", "\\\\(")
  ns <- str_replace_all(ns, "\\)", "\\\\)")
  index <- str_locate_all(text, ns)
  index <-sort(unlist(index))
  starts <- index[1:length(index)%%2 != 0]
  ends <- lead(starts)
  actual_end <- str_locate(eu, "$")[1,2]
  ends[length(ends)] <- actual_end
  speeches <- str_sub(text, start = starts, end = ends)
  
  speech_tib <- df %>% add_column(speeches)
  for (i in seq_along(speech_tib$speeches)){  
    speech_tib$speeches[i] <- str_sub(speech_tib$speeches[i], start = (str_count(names_search[i])+4))
  }
  
  if (rm_shouts) {
    speech_tib %<>% filter(!str_detect(speeches, "My Lords[^, ]"))
  }
  speech_tib
}

reduce_speeches <- function(df = appearances_tib) {
  full_speeches <- df %>% group_by(name) %>% nest() %>% mutate(all_speeches = map(data, "speeches"),
                                                               party = map(data, "party"),
                                                               gender = map(data, "gender"),
                                                               party = map_chr(party, unique),
                                                               gender = map_chr(gender, unique))
  
  join_speeches <- function(x) str_c(x, collapse = " ")
  full_speeches %<>% mutate(all_speeches = map_chr(all_speeches, join_speeches)) %>% select(-data)
} 


get_parties <- function(name_vec) {
  
  tib <- tibble(name = name_vec) 
  tib %<>% mutate(name = ifelse(str_count(name, "\\(") > 1, str_sub(str_extract(name, "\\([^)]+\\)"), 2, -2), name)) %>%
    mutate(containing_party = str_detect(name, "\\("))
  
  
  tib_party <- filter(tib, containing_party == TRUE)
  tib_party <- tibble(
    name = str_extract(tib_party$name, "[a-zA-Z ',\\-]+"),
    party = str_extract(tib_party$name, "\\([^()]+\\)")
  )
  tib_party <- unique(tib_party)
  tib$name <- str_replace(tib$name, "\\(.+\\)", "")
  tib$name <- trimws(tib$name)
  tib_party$name <- trimws(tib_party$name)
  
  # If there are lords with missing parties, search for these in wikipedia table
  wiki_ids <- load_lord_wiki_data()
  
  ID_tib <- left_join(tib, tib_party) %>% select(name, party)
  surname_join <- tibble(name = unique(ID_tib$name), surname = get_surnames(unique(ID_tib$name)))
  
  # Get lords missing a party assignment
  missing <- ID_tib %>% filter(is.na(party)) %>% unique() 
  
  # If they're a bishop, mark them as lord spiritual
  missing %<>% mutate(party = ifelse(str_detect(name, "Bishop|Archbishop"), "spiritual", party))
  
  # otherwise search the wiki table for the lord's name   
  missing_party_join <- tibble(name = wiki_ids$name[which(wiki_ids$name %in% missing$name)], party_wiki = wiki_ids$party[which(wiki_ids$name %in% missing$name)])                
  missing %<>% left_join(missing_party_join) %>%  mutate(party = ifelse(is.na(party), party_wiki, party)) %>% select(-party_wiki)

  # Party abbreviations
  abbs <- tibble(party = c("Conservative", "Labour", "Liberal Democrats", "Crossbencher", "Ulster Unionist", "Green", "Plaid Cymru", "spiritual"), party_missing =  c("(Con)", "(Lab)", "(LD)", "(CB)", "(UUP)", "(GP)", "(PC)", "(spiritual)"))
  
  missing %<>% left_join(abbs) %>% select(-party)
  
  # return the ID tibble (name + party ordered by appearance)
  # but remember there may still be missing party assignments
  ID_tib %<>% left_join(missing) %>% mutate(party = ifelse(is.na(party), party_missing, party)) %>% select(-party_missing)
  if (sum(is.na(ID_tib$party)) > 0) {
    print("Warning: some Lords are missing a party assignment")
  }
  return(ID_tib)
}

# Adds surnames to a dataframe containing a "name" variable
add_surname <- function(df) {
  surname_join <- tibble(name = unique(df$name), surname = get_surnames(unique(df$name)))
  df %<>% left_join(surname_join)
}

# Could scrape this directly rather than from local tables. 
load_lord_wiki_data <- function() {
  lords <- readRDS("data//lords_table.RDAT")
  deceased <- readRDS("data//lords_deceased_table.RDAT")
  loa <- readRDS("data//lords_leave_table.RDAT")
  lords %<>% select(Peer, Party) %>% rename(name = Peer, party = Party)
  deceased %<>% select(Name, Party) %>% rename(name = Name, party = Party)
  loa %<>% select(Lord, Party) %>% rename(name = Lord, party = Party)
  wiki_ids <- bind_rows(lords, deceased, loa)  
}

`%!in%` <- negate(`%in%`)

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
  #pulled_names <- full_speeches$name %>% str_match("^(?:\\w+\\s)(\\w+[-']?\\w+)(\\W\\w+(?!of))?") 
  pulled_names <- names %>% str_match("^(?:\\w+\\s)(\\w+[-']?\\w+)(\\W\\w+(?!of))?") 
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

get_seq <- function(vec) {
  grp_cnt <- NULL
  count = 0
  i = 1
  while (i < length(vec)) {
    count  = 0
    j = i
    while (j < length(vec)) {
      if (is.na(vec[i]) | is.na(vec[j])) {
        break
      }
      if (vec[i] == vec[j]) {
        count = count + 1
      } else  if (count > 1) {
        i = i + count - 1
        break
      } else {
        break
      }
      j = j + 1
    }
    grp_cnt <- c(grp_cnt, count)
    i = i + 1
  }
  grp_cnt
}


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
