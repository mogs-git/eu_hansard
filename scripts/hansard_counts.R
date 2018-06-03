# Non-text analysis: raw counts

# Inputs
full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances.RDAT")

pacman::p_load(tidyverse, magrittr, tidytext)
`%!in%` <- negate(`%in%`)

# team composition ----

# PLOT: Number of members of each party coloured by gender.
### output ####
party_numbers_plot <- full_speeches %>% group_by(party, gender) %>% summarise(n = n()) %>%
  ggplot(aes(party, n)) +
  geom_col(aes(fill = gender)) +
  ylab("# members") +
  scale_y_continuous(breaks = seq(0,30,5))
### output ###

# word counts by gender ----

# Count number of words in each speech (SUMMARY)
(words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
   group_by(r, name) %>% summarise(word_count = n()))

# Count total number of words said by each lord
(words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name)))

# Look for pattern in order of speeches
### output ####
seq_plot <- words_per_speech %>% left_join(distinct(appearances_tib, name, party)) %>% ggplot(aes(r, word_count)) + geom_col(aes(fill = party), width = 1) + scale_fill_brewer(palette = "Set1")
seq_plot #+ coord_cartesian(xlim = c(200,250))
### output ###

# This will be more meaningful later when I can split up text by topic modelling, but for now
# look at who controls what portion of debate by n_speeches

get_quartiles <- function(df) {
  nr <- df %>% nrow(.)
  nr <- c(1:4) * nr/4 
  nr %<>% round(0)
  dnr <- diff(nr)
  section <- c(rep("1st", nr[1]), map2(c("2nd", "3rd", "4th"), dnr, rep)) %>% unlist()
}
sections <- get_quartiles(appearances_tib)
mycols = c("blue4", "chartreuse4", "gold1", "red3", "grey")

### output ####
engagement_per_quarter <- appearances_tib %>% mutate(sec = sections) %>% group_by(sec, party) %>% count() %>%
  ggplot(aes(party, n)) + geom_col(aes(fill = party)) + facet_wrap(~sec) + scale_fill_manual(values = mycols)

# Speeches as a proportion of total made that day by the party
party_numbers <- full_speeches %>% keep_main_parties() %>% group_by(party_f) %>% summarise(party_total = length(party_f)) 
party_speeches <- appearances_tib %>% group_by(party_f) %>% summarise(party_total = length(party_f)) 
engagement_per_quarter_prop <- appearances_tib %>% mutate(sec = sections) %>% group_by(sec, party_f) %>% count() %>% 
  left_join(party_speeches) %>% mutate(p = n/party_total) %>%
  ggplot(aes(party_f, p)) + geom_col(aes(fill = party_f)) + facet_wrap(~sec) + scale_fill_manual(values = mycols)
### output ###


appearances_tib %>% 
  ggplot(aes(r, 1)) + geom_col(aes(fill = party_f)) + scale_fill_manual(values = c(mycols[1], "white", "white", mycols[4], "white")) + theme_classic()


# running mean of speech lengths
wcs <- words_per_speech$word_count
running_mean <- NULL
for (i in seq_along(wcs)) {
  running_mean[i] <- mean(wcs[1:i])
}
rm_tib <- tibble(id = 1:247, running_mean)
ggplot(rm_tib, aes(id, running_mean)) + geom_point() + geom_line()

# with window
running_mean <- function(vec, window) {
  indexes <- which((1:length(vec))%%window == 0)
  m <- vector("numeric", length(indexes))
  for (i in seq_along(indexes)) {
    for (j in 1:window) {
      m[i] = m[i] + vec[indexes[i] - (j - 1)]
    }
  }
  rm_tib <- tibble(id = indexes, running_mean = m)
  rm_tib
}
indexes <- which((1:length(wcs))%%3 == 0)
m <- vector("numeric", length(indexes))
for (i in seq_along(indexes)) {
  m[i] = wcs[indexes[i]] + wcs[indexes[i]-1] + wcs[indexes[i]-2]
}
rm_tib <- tibble(id = indexes, running_mean = m)
ggplot(rm_tib, aes(id, running_mean)) + geom_point() + geom_line()

running_mean(wcs, 5) %>% ggplot(aes(id, running_mean)) + geom_point() + geom_line()


# gender distribution in sorted word counts
words_per_lord %<>% add_gender()
words_per_lord %<>% mutate(name =  as.factor(name))
### output ####
words_per_lord %>% 
  ggplot(aes(x = fct_reorder(name, word_count), y = word_count)) + geom_col(aes(fill = gender)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
### output ###

# ladies look evenly distributed among speech lengths

# how often did each party speak?

speeches_by_party <- appearances_tib %>% group_by(party) %>% summarise(n_speeches = n())

# how often would you expect each to speak based on number of lords present?

exp_speeches <- appearances_tib %>% distinct(name, party) %>% group_by(party) %>% summarise(n = n()) %>%
  mutate(total_speeches = 247, nprob = n/sum(n), expected_nspeeches = round(total_speeches*nprob)) 

speeches_by_party %>% left_join(exp_speeches, by = "party") %>% select(party,n_speeches,expected_nspeeches) %>% 
  gather(speech_type, count, n_speeches, expected_nspeeches) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = speech_type), position = "dodge")

# word counts

words_per_speech <- appearances_tib %>% mutate(r = row_number()) %>% unnest_tokens(word, speeches) %>% 
  group_by(r, name) %>% summarise(word_count = n()) %>% filter(!is.na(name)) %>% ungroup() %>% add_gender()

words_per_lord <- words_per_speech %>% group_by(name) %>% summarise(word_count = sum(word_count)) %>% filter(!is.na(name))

words_per_lord %<>% add_gender()
words_per_lord %<>% mutate(name =  as.factor(name))
wordCountPlot <- words_per_lord %>% ggplot(aes(x = fct_reorder(name, word_count), y = word_count)) + geom_col(aes(fill = gender)) 

wordExpectationPlot <- words_per_lord %>% group_by(gender) %>% summarise(total_words = sum(word_count), total_speakers = length(name)) %>% ungroup() %>%
  mutate(tot_s = sum(total_speakers), tot_w = sum(total_words), exp = tot_w*(total_speakers/tot_s)) %>% gather(type, count, total_words, exp) %>%
  ggplot(aes(gender, count)) + geom_col(aes(fill = type), position = 'dodge')

speechesExpectationPlot <- appearances_tib %>% group_by(gender) %>% summarise(n_speeches = length(name), n_speakers = length(unique(name))) %>% ungroup() %>% 
  mutate(tot_s = sum(n_speeches), exp = tot_s * n_speakers/sum(n_speakers)) %>%
  gather(type, count, n_speeches, exp) %>%
  ggplot(aes(gender, count)) + geom_col(aes(fill = type), position = 'dodge')

words_per_speech %>% group_by(gender) %>% arrange(gender, word_count) %>% mutate(r = row_number(), p = r/length(r)) %>% ggplot(aes(word_count, p)) + geom_line(aes(colour = gender)) + geom_point(aes(colour = gender))

words_per_lord %>% group_by(gender) %>% arrange(gender, word_count) %>% mutate(r = row_number(), p = r/length(r)) %>% ggplot(aes(word_count, p)) + geom_line(aes(colour = gender)) + geom_point(aes(colour = gender))

words_per_speech %<>% left_join(party_id)
words_per_speech %>% filter(party %in% main_parties) %>% group_by(party) %>% arrange(party, word_count) %>% mutate(r = row_number(), p = r/length(r)) %>% ggplot(aes(word_count, p)) + geom_line(aes(colour = party)) + geom_point(aes(colour = party))

words_per_lord %>% left_join(party_id) %>% group_by(party) %>% summarise(total_words = sum(word_count), total_speakers = length(name)) %>% ungroup() %>%
  mutate(tot_s = sum(total_speakers), tot_w = sum(total_words), exp = tot_w*(total_speakers/tot_s)) %>% gather(type, count, total_words, exp) %>%
  ggplot(aes(party, count)) + geom_col(aes(fill = type), position = 'dodge')

library(ggpubr)
#### output ###
ggarrange(ggarrange(speechesExpectationPlot, wordExpectationPlot, ncol = 2), wordCountPlot, nrow = 2)
#### output ###