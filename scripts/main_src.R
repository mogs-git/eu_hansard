# Hansard Functions

# All the times a name appears in order of appearance
extract_names <- function(text, prefix_string = "\r\n\r\n((Lord|The Advocate|The Countess|The Archbishop|The Parliamentary|Viscount|Baroness|^Noble Lords$)(.+))(\r\n)") {
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
    if (str_detect(df$name[i], "Lord|Viscount|Archbishop|Keen|Bridges")) {
      gender_vector[i] <- "male"
    } else {
      gender_vector[i] <- "female"
    }
  }
  
  df %<>% add_column(gender = gender_vector)
}

# Extract the speech made by each lord in chronological order

bind_speeches <- function(df = appearances_tib, text = eu) {
  
  names_search <- text %>% str_extract_all("\r\n\r\n((Lord|The Advocate|The Countess|The Archbishop|The Parliamentary|Viscount|Baroness|^Noble Lords$)(.+))(\r\n)") %>%
    unlist() 
  names_search_u <- unique(names_search)
  ns <- str_c(names_search_u, collapse = "|")
  ns <- str_replace_all(ns, "\\(", "\\\\(")
  ns <- str_replace_all(ns, "\\)", "\\\\)")
  index <- str_locate_all(text, ns)
  index <-sort(unlist(index))
  starts <- index[1:length(index)%%2 != 0]
  ends <- lead(starts)
  speeches <- str_sub(text, start = starts, end = ends)
  
  speech_tib <- df %>% add_column(speeches)
  for (i in seq_along(speech_tib$speeches)){  
    speech_tib$speeches[i] <- str_sub(speech_tib$speeches[i], start = (str_count(names_search[i])+4))
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
