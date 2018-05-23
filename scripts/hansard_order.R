# Get lenght of speeches
appearances_tib_f <- appearances_tib %>% filter(!str_detect(speeches, "My Lords[^, ]"))
# filter out cases of less than 50 characters

# extract sequence of parties
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
  geom_col(aes(index, run, fill = party), width = 1)

ggplot(party_seq_tib) +
  geom_histogram(aes(run, fill = party))


full_speeches %<>% mutate(party_f = ifelse(party %in% main_parties, party, 'other'))
words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>%
  filter(party != next_party) %>% mutate(s = next_count < word_count + 50) %>% group_by(party, next_party) %>%
  summarise(s = sum(s)) %>% arrange(desc(s))

# take top 10% of talks, who follows who?
words_per_speech %>% 
  left_join(select(full_speeches, name, party)) %>% ungroup() %>% 
  mutate(next_party = lead(party), next_count = lead(word_count)) %>% top_n(25, next_count) %>%
  group_by(party, next_party) %>%
  count() %>% arrange(desc(n))
