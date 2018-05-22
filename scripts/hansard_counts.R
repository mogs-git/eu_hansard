# Non-text analysis: raw counts

# Inputs
full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")

`%!in%` <- negate(`%in%`)

# team composition ----

# PLOT: Number of members of each party coloured by gender.
full_speeches %>% group_by(party, gender) %>% summarise(n = n()) %>%
  ggplot(aes(party, n)) +
  geom_col(aes(fill = gender)) +
  ylab("# members") +
  scale_y_continuous(breaks = seq(0,30,5))

# word counts by gender ----

# Count number of words in each speech (SUMMARY)
(words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
   group_by(r, name) %>% summarise(word_count = n()))

# Count total number of words said by each lord
(words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name)))

# Look for pattern in order of speeches
seq_plot <- words_per_speech %>% left_join(distinct(appearances_tib, name, party)) %>% ggplot(aes(r, word_count)) + geom_col(aes(fill = party), width = 1) + scale_fill_brewer(palette = "Set1")
seq_plot #+ coord_cartesian(xlim = c(200,250))

# This will be more meaningful later when I can split up text by topic modelling, but for now
# look at who controls what portion of debate by n_speeches

nr <- appearances_tib %>% nrow(.)
nr <- c(1:4) * nr/4 
nr %<>% round(0)
dnr <- diff(nr)
section <- c(rep("1st", nr[1]), map2(c("2nd", "3rd", "4th"), dnr, rep)) %>% unlist()
mycols = c("blue4", "chartreuse4", "gold1", "red4", "grey")
appearances_tib %>% mutate(sec = section) %>% group_by(sec, party_f) %>% count() %>%
  ggplot(aes(party_f, n)) + geom_col(aes(fill = party_f)) + facet_wrap(~sec) + scale_fill_manual(values = mycols)
  

appearances_tib %>% 
  ggplot(aes(r, 1)) + geom_col(aes(fill = fct_rev(party_f))) + scale_fill_brewer(palette = "Spectral")

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

# running mean of speech lengths
wcs <- words_per_speech$word_count
running_mean <- NULL
for (i in seq_along(wcs)) {
  running_mean[i] <- mean(wcs[1:i])
}
rm_tib <- tibble(id = 1:247, running_mean)
ggplot(rm_tib, aes(id, running_mean)) + geom_point() + geom_line()

# gender distribution in sorted word counts
words_per_lord %<>% add_gender()
words_per_lord %<>% mutate(name =  as.factor(name))
words_per_lord %>% 
  ggplot(aes(x = fct_reorder(name, word_count), y = word_count)) + geom_col(aes(fill = gender)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ladies look evenly distributed among speech lengths

# how often did each party speak?

speeches_by_party <- appearances_tib %>% group_by(party) %>% summarise(n_speeches = n())

# how often would you expect each to speak based on number of lords present?

exp_speeches <- appearances_tib %>% distinct(name, party) %>% group_by(party) %>% summarise(n = n()) %>%
  mutate(total_speeches = 247, nprob = n/sum(n), expected_nspeeches = round(total_speeches*nprob)) 

speeches_by_party %>% left_join(exp_speeches) %>% select(party,n_speeches,expected_nspeeches) %>% 
  gather(speech_type, count, n_speeches, expected_nspeeches) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = speech_type), position = "dodge")

