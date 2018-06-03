# load----
pacman::p_load(tidyverse, magrittr, tidytext, stringr, ggpubr)

full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")
appearances_tib_f <- readRDS("data//appearances_f.RDAT")
`%!in%` <- negate(`%in%`)
party_cols <- c("dodgerblue3", "chartreuse3", "goldenrod1", "firebrick2", "grey30")


# Get lenght of speeches
# filter out cases of less than 50 characters

# extract sequence of parties
appearances_tib <- appearances_tib_f %>% select(-party) %>% rename(party = party_f)
party_seq <- appearances_tib$party
party_seq[which(is.na(party_seq))] <- "other"
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
party_seq_values <- get_seq(party_seq)
party_seq_index <- cumsum(party_seq_values)
party_seq_tib <- tibble(party = party_seq[party_seq_index], run = party_seq_values)
ggplot(party_seq_tib) +
  geom_col(aes(x = party, y = run))

party_seq_tib %<>% mutate(index = row_number())

ggplot(party_seq_tib) +
  geom_col(aes(index, run, fill = party), width = 1) +
  scale_fill_manual(values = party_cols) + 
  theme_pubr()

ggplot(party_seq_tib) +
  geom_histogram(aes(run, fill = party)) +
  scale_fill_manual(values = party_cols) + 
  theme_pubr()

appearances_tib_fp %>%
  mutate(speech_id = row_number()) %>%
  ggplot(aes(speech_id, 1)) +
  geom_col(aes(fill = party), width = 1) + 
  scale_fill_manual(values = party_cols) + 
  theme_pubr()

# similar idea, but with words
appearances_tib_fp %>% 
  unnest_tokens(word, speeches) %>%
  mutate(r = row_number()) %>%
  ggplot(aes(r, 1)) + 
  geom_col(aes(fill = party), width = 1) + 
  scale_fill_manual(values = party_cols) + 
  theme_pubr()


tokenised_df <- readRDS("data/tokenised_df.RDAT")

tokenised_df <- readRDS("data/tokenised_df.RDAT")
party_id_tib <- readRDS("data/party_id_tib.RDAT")
party_id <- party_id_tib %>% transmute(name = name, party = party_f)

(words_per_speech <- tokenised_df %>%
    group_by(speech_id, name) %>% 
    summarise(word_count = n()))

# Who says more than the preceding guy most often?
words_per_speech %>% 
  left_join(party_id_tib) %>% ungroup() %>%
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% 
  mutate(s = next_count < word_count + 50) %>% group_by(party, next_party) %>%
  summarise(s = sum(s)) %>% arrange(desc(s)) %>%
  unite(pair, party, next_party) %>%
  ggplot(aes(fct_reorder(pair, s), s)) + geom_col() + theme(axis.text.x=element_text(angle=45, hjust=1))

words_per_speech %>% 
  left_join(party_id) %>% ungroup() %>%
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% filter(!is.na(next_party)) %>%
  mutate(s = next_count < word_count + 50) %>% group_by(party, next_party) %>%
  summarise(s = sum(s)) %>% arrange(desc(s)) %>% # spread(party, s)
  ggplot() + geom_tile(aes(party, next_party, fill = s))

# take top 10% of talks, who follows who?
words_per_speech$word_count %>% quantile()

words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% top_n(25, next_count) %>%
  group_by(party, next_party) %>%
  count() %>% arrange(desc(n))

# pairs where response made was over 75th quantile in terms of length.
words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% filter(next_count > 420) %>%
  group_by(party, next_party) %>%
  count() %>% arrange(desc(n))

words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>%
  ggplot(aes(word_count, next_count)) + geom_point()
