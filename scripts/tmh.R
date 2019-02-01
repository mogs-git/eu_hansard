pacman::p_load(tidyverse, magrittr, tidytext) # igraph, ggraph, tidygraph, grid, gridExtra, ggpubr, networkD3)

full_speeches <- readRDS("data//full_speeches.RDAT")
appearances_tib <- readRDS("data//appearances_2.RDAT")

full_speeches$all_speeches %<>% map(~str_replace_all(., "\\\r\\\n\\\r\\\n", " "))
appearances_tib$speeches %<>% map(~str_replace_all(., "\\\r\\\n\\\r\\\n", " "))

appearances_tib %<>% mutate(speech_id = row_number())

at_words <- appearances_tib %>%
  unnest_tokens(word, speeches) %>%
  count(speech_id, word, sort = TRUE) %>%
  ungroup() %>%
  anti_join(stop_words)

at_words

at_total <- at_words %>%
  group_by(speech_id) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))

at_total

fs_words <- full_speeches %>%
  unnest_tokens(word, all_speeches) %>%
  count(name, word, sort = TRUE) %>%
  ungroup()

fs_words

fs_counts <- fs_words %>%
  group_by(name) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total))

fs_counts


# Merge with Number of speeches

speech_counts <- appearances_tib %>%
  group_by(name) %>%
  summarise(n_speeches = n(), party = unique(party_f)) %>%
  arrange(desc(n_speeches))

fs_counts %>% 
  left_join(speech_counts) %>%
  ggplot(aes(n_speeches, total)) + 
  geom_boxplot(aes(group = n_speeches)) + 
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  xlab("Number of speeches") +
  ylab("Total number of words")


# More explicitly

fs_counts %>% 
  left_join(speech_counts) %>%
  mutate(words_per_speech = total/n_speeches) %>%
  ggplot(aes(words_per_speech)) + 
  geom_histogram() +
  scale_y_continuous(breaks = c(seq(0,10,2)))

fs_counts %>% 
  left_join(speech_counts) %>%
  mutate(words_per_speech = total/n_speeches) %>%
  ggplot(aes(n_speeches, words_per_speech)) + 
  geom_point() 

at_words %>%
  group_by(speech_id) %>%
  summarise(total = sum(n)) %>%
  left_join(distinct(appearances_tib, name, speech_id)) %>%
  left_join(speech_counts) %>%
  left_join(rename(fs_counts, word_count = total)) %>%
  filter(n_speeches > 4) %>%
  mutate(words_per_speech = word_count/n_speeches) %>%
  ggplot(aes(fct_reorder(name, n_speeches), total, colour = as.factor(n_speeches))) + 
  geom_boxplot() + 
  geom_point(alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# The most frequent words used in each speech

at_total %>% ggplot(aes(total)) + stat_ecdf()

at_words <- at_words %>% left_join(at_total)

small_speeches <- filter(at_total, total <= 50)

at_words %<>% filter(total > 50)

at_words <- at_words %>%
  bind_tf_idf(word, speech_id, n)
at_words

at_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

top_speeches <- at_words %>%
  distinct(speech_id, total) %>% 
  arrange(desc(total)) %>%
  head(6)

at_words %>%
  semi_join(top_speeches) %>%
  arrange(desc(tf_idf)) %>%
  group_by(speech_id) %>% 
  top_n(15, wt=tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = speech_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speech_id, ncol = 2, scales = "free") +
  coord_flip()

# I don't like this, it's too broad and unclear that the words really represent the speech

at_words_reduced <- at_words %>%
  filter(n > 2) %>%
  group_by(speech_id) %>%
  top_n(5, wt=tf_idf) %>%
  ungroup()

unique_words <- at_words_reduced %>%
  count(word) %>%
  mutate(unique = nn == 1) %>%
  filter(unique, !str_detect(word, "[0-9]*[0-9]"))

unique_words

at_words_reduced %>%
  semi_join(unique_words) %>%
  arrange(desc(tf_idf)) %>%
  print(.) %>%
  ggplot(aes(total, tf_idf)) + geom_point(aes(colour = tf, size = idf))

at_words_reduced %>% 
  semi_join(unique_words) %>%
  arrange(desc(tf_idf)) %>%
  filter(total > 200, tf_idf > 0.08)

library(ggrepel)

at_words %>%
  filter(n > 2) %>%
  group_by(speech_id) %>%
  top_n(1, wt=tf_idf) %>%
  ungroup() %>%
  arrange(speech_id) %>% 
  ggplot(aes(speech_id, tf_idf)) + ggrepel::geom_text_repel(aes(label=word))

at_words %>%
  filter(!str_detect(word, "[0-9]*[0-9]")) %>%
  filter(n > 3) %>%
  group_by(speech_id) %>%
  top_n(1, wt=tf_idf) %>%
  ungroup() %>%
  arrange(speech_id) %>% 
  mutate(row=row_number()) %>%
  ggplot(aes(speech_id, 1)) + 
  geom_point(color = "red") +
  geom_text_repel(
    aes(label=word, size = tf_idf),
    nudge_y      = 0.06,
    direction    = "y",
    angle        = 0,
    vjust        = 0,
    segment.size = 0.1
  ) +
  scale_size(range = c(3,5)) +
  xlab("speech number") + 
  ylim(c(0.975,1.075)) 

# at_words %>%
#   filter(!str_detect(word, "[0-9]*[0-9]")) %>%
#   filter(n > 3) %>%
#   group_by(speech_id) %>%
#   top_n(3, wt=tf_idf) %>%
#   mutate(pos = row_number(), pos = ifelse(pos == 4, 3, pos), pos = 4-pos) %>%
#   ungroup() %>%
#   arrange(speech_id) %>% 
#   mutate(row=row_number()) %>%
#   ggplot(aes(speech_id, pos)) + 
#   geom_point(aes(color = as.factor(pos))) +
#   geom_text_repel(
#     aes(label=word, size = tf_idf),
#     nudge_y      = 0,
#     direction    = "y",
#     angle        = 0,
#     vjust        = 0,
#     segment.size = 0.1
#   ) +
#   scale_size(range = c(3,5))

# Can we combine this with the sentiment of the speech?

bing <- get_sentiments("bing") # least "wishy-washy" of the sentiment libraries

tidy_tib <- appearances_tib %>%
  unnest_tokens(word, speeches) %>%
  ungroup()

my_stop_words <- c("lord", "lords", "noble", "baroness", "lordship")

tidy_tib %<>%
  anti_join(stop_words) %>%
  filter(!word %in% my_stop_words) %>%
  left_join(bing) 

tidy_tib %>%
  count(sentiment)

sentiments <- tidy_tib %>%
  filter(!is.na(sentiment)) %>%
  group_by(speech_id) %>%
  summarise(n_pos = sum(str_detect(sentiment, "positive")),
            n_neg = sum(str_detect(sentiment, "negative")),
            tot = n())

speeches_used_in_plot <- at_words %>%
  filter(!str_detect(word, "[0-9]*[0-9]")) %>%
  filter(n > 3) %>%
  group_by(speech_id) %>%
  top_n(1, wt=tf_idf) %>%
  ungroup() %>%
  pull(speech_id)

sentiments %<>% filter(speech_id %in% speeches_used_in_plot)

sentiments %<>%
  mutate(diff = (n_pos - n_neg) / log(tot))

# Make the gnarly plot again

at_words %>%
  filter(!str_detect(word, "[0-9]*[0-9]")) %>%
  filter(n > 3) %>%
  group_by(speech_id) %>%
  top_n(1, wt=tf_idf) %>%
  ungroup() %>%
  left_join(select(sentiments, speech_id, diff)) %>%
  arrange(speech_id) %>% 
  mutate(row=row_number()) %>%
  ggplot(aes(speech_id, diff)) + 
  geom_point(color = "red") +
  geom_text_repel(
    aes(label=word, size = tf_idf),
    nudge_y      = 0.06,
    direction    = "y",
    angle        = 0,
    vjust        = 0,
    segment.size = 0.1
  ) +
  scale_size(range = c(3,5)) +
  xlab("speech number") 

# I don't really like it, here's a non chronological version:

at_words %>%
  filter(!str_detect(word, "[0-9]*[0-9]")) %>%
  filter(n > 3) %>%
  group_by(speech_id) %>%
  top_n(1, wt=tf_idf) %>%
  ungroup() %>%
  left_join(select(sentiments, speech_id, diff)) %>%
  arrange(speech_id) %>% 
  mutate(row=row_number()) %>%
  ggplot(aes(fct_reorder(as.factor(speech_id), diff), diff)) + 
  geom_point(color = "red") +
  geom_line(color = "red") +
  geom_text_repel(
    aes(label=word, size = tf_idf),
    nudge_y      = 0.06,
    direction    = "y",
    angle        = 0,
    vjust        = 0,
    segment.size = 0.1
  ) +
  scale_size(range = c(3,5)) +
  xlab("speech number") 

# Also don't really like this. DEAD END?

# If you look back at the speeches, sentiment analysis really has some problems too. Perhaps I can talk about sentiment analysis in a followup.

# Counting specific words and features of the text. 

tidy_tib %<>% select(-sentiment)

tidy_tib %>%
  filter(str_detect(word, "agree|disagree"))

# bigrams and trigrams

fs_bigrams <- full_speeches %>% 
  mutate(all_speeches = map_chr(all_speeches, as.vector)) %>%
  unnest_tokens(bigram, all_speeches, token = "ngrams", n = 2)

bigrams_separated <- fs_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_filtered %>%
  filter(word1 == "amendment",
         str_detect(word2, "^[0-9]")) %>%
  unite(bigram, c(word1, word2), sep=" ") %>%
  count(name, bigram)

bigrams_filtered %>%
  filter(word1 %in% c("lord","baroness","lady"),
         word2 %!in% c("lord", "lady")) %>%
  unite(bigram, c(word1, word2), sep=" ") %>%
  count(name, bigram) 

bigram_tf_idf <- bigrams_united %>%
  count(name, bigram) %>%
  bind_tf_idf(bigram, name, n) %>%
  arrange(desc(tf_idf))

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# trigrams

trigrams <- full_speeches %>% 
  mutate(all_speeches = map_chr(all_speeches, as.vector)) %>%
  unnest_tokens(trigram, all_speeches, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, "lord"),
         !word2 %in% c(stop_words$word, "lord"),
         !word3 %in% c(stop_words$word, "lord")) %>%
  count(word1, word2, word3, sort = TRUE)

trigrams

# Correlations among words

hayter_words <- appearances_tib %>%
  filter(name == "Baroness Hayter of Kentish Town") %>%
  unnest_tokens(word, speeches) %>%
  filter(!word %in% stop_words$word) %>%
  select(name, speech_id, word)

library(widyr)
# count words co-occuring within sections
word_pairs <- hayter_words %>%
  pairwise_count(word, speech_id, sort = TRUE)
#devtools::install_github("juliasilge/tidytext")

word_pairs # that appear together within each speech

word_cors <- hayter_words %>%
  group_by(word) %>%
  filter(n() >= 2) %>%
  pairwise_cor(word, speech_id, sort = TRUE)

word_cors

# Words in context, followed by sentiment analysis or POS tagging

eu_sents <- appearances_tib %>%
  mutate(speeches = str_to_lower(speeches),
         eu_sents = wordInContext_sentence(speeches, "eu"),
         num_sents = map_int(eu_sents, length)) %>%
  select(-speeches)

eu_sents$eu_sents[[1]]

tagged_eu_sents <- eu_sents %>%
  unnest() %>%
  mutate(tagged_sents = map(eu_sents, ~anno(., ".*"))) %>%
  group_by(speech_id) %>%
  mutate(sent_id = row_number())

tagged_eu_sents[1,] %>% unnest() 
 
annos_nested <- appearances_tib_fp %>% 
  mutate(speech_id = row_number()) %>%
  group_by(speech_id) %>%
  nest() %>%
  mutate(s = map(data, "speeches")) %>%
  mutate(annotations = map(s, anno, ".*"))

annos_nested$annotations[[1]]  %>% distinct(word, tags)

annos_nested$annotations[[1]] %>% filter(!str_detect(word, "\\W")) %>%
  mutate(nword = lead(word), ntag = lead(tags), ndesc = lead(description)) %>%
  unite(bigram, word, nword, sep = " ") %>%
  unite(bitag, tags, ntag, sep = " ") %>% 
  filter(str_detect(bitag, "JJ[S|R]? NN[S|P|PS]?")|str_detect(bitag, "NN[S|P|PS]? NN[S|P|PS]?")) %>% View()

### lexrank

library(lexRankr)
library(dplyr)

df <- tibble(doc_id = 1:3, text = c("Testing the system. Second sentence for you.", "System testing the tidy documents df.", "Documents will be parsed and lexranked."))

df %>% unnest_sentences(sents, text) %>% bind_lexrank(sents, doc_id, level = 'sentences') %>% arrange(desc(lexrank)) #### lexRank applied to a charcter vector of documents library(lexRankr)

text <- c("Testing the system. Second sentence for you.", 
          "System testing the tidy documents df.", 
          "Documents will be parsed and lexranked.")
lexRank(text)

appearances_tib[1:5,] %>%
  mutate(central_sent = map(speeches, lexRank)) %>% View()

# LDA ----

library(topicmodels)

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

my_stop_words <- c("lord", "lords", "noble", "baroness", "lordship", "minister", "amendment", "parliament", "eu", "uk", "government", "european", "union", "united", "kingdom")

at_words <- appearances_tib %>%
  unnest_tokens(word, speeches) %>%
  anti_join(stop_words) %>%
  filter(!word %in% my_stop_words, !str_detect(word, "[0-9]")) %>%
  count(speech_id, word, sort = TRUE) %>%
  ungroup() 

speeches_dtm <- at_words %>%
  cast_dtm(speech_id, word, n)

speeches_dtm

speeches_lda <- LDA(speeches_dtm, k = 4, control = list(seed = 1234))

speeches_lda

speech_topics <- tidy(speeches_lda, matrix = "beta")

speech_topics


top_terms <- speech_topics %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

View(top_terms)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

speeches_gamma <- tidy(speeches_lda, matrix = "gamma") %>% rename(speech_id = document)

speeches_gamma

# Topic 1 = citizens rights; 2 = nuclear; 3 = womens/workers rights; 4 = devolved admins

speeches_gamma %>% ggplot() + geom_point(aes(x = speech_id, y = gamma)) + facet_wrap(~topic)

speeches_gamma %>%
  group_by(speech_id) %>%
  filter(gamma == max(gamma)) %>%
  ggplot(aes(as.numeric(speech_id), 1)) + 
  geom_point(color = "red") +
  geom_text_repel(
    aes(label=topic, size = gamma),
    nudge_y      = 0.06,
    direction    = "y",
    angle        = 0,
    vjust        = 0,
    segment.size = 0.1
  ) +
  scale_size(range = c(3,5)) +
  xlab("speech number") + 
  ylim(c(0.975,1.075)) +
  theme(legend.position="none")


speeches_gamma %>%  
  group_by(speech_id) %>%
  filter(gamma == max(gamma)) %>%
  ggplot(aes(speech_id, gamma)) + geom_point()

speeches_gamma %>%  
  group_by(speech_id) %>%
  filter(gamma == max(gamma)) %>%
  ggplot(aes(gamma)) + geom_histogram()

sketchy_speeches <- speeches_gamma %>% 
  group_by(speech_id) %>%
  filter(gamma == max(gamma)) %>%
  filter(gamma < 0.95) %>%
  ungroup() %>%
  mutate(speech_id = as.numeric(speech_id))

appearances_tib %>%
  inner_join(sketchy_speeches) %>% 
  `[`(2,) %>% pull(speeches)

speeches_gamma %>%
  filter(speech_id %in% sketchy_speeches$speech_id) %>%
  ggplot(aes(topic, gamma)) + geom_col(aes(fill = as.factor(topic))) + facet_wrap(~speech_id)


# Trigram ----

tidy_tib %>%
  filter(str_detect(word, "agree|disagree"))

bigrams_separated %>%
  filter(word1 == "agree", word2 != "to")

fs_trigrams <- full_speeches %>% 
  mutate(all_speeches = map_chr(all_speeches, as.vector)) %>%
  unnest_tokens(trigram, all_speeches, token = "ngrams", n = 3)

trigrams_separated <- fs_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_separated

trigrams_separated %>%
  filter(word1 != "not", word2 == "agree", word3 != "to")

trigrams_separated %>%
  filter(word1 != "not", word2 == "disagree", word3 != "to")

trigrams_separated %>%
  filter(word1 %in% c("is", "are"), word2 == "right")

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

bigrams_filtered %>%
  filter(word1 == "amendment",
         str_detect(word2, "^[0-9]")) %>%
  unite(bigram, c(word1, word2), sep=" ") %>%
  count(name, bigram)

bigrams_filtered %>%
  filter(word1 %in% c("lord","baroness","lady"),
         word2 %!in% c("lord", "lady")) %>%
  unite(bigram, c(word1, word2), sep=" ") %>%
  count(name, bigram) 

# questions

appearances_tib <- appearances_tib %>%
  mutate(speeches = map(speeches, ~str_to_lower(.)))

for (i in 1: dim(appearances_tib)[1]) {
  print(i)
  appearances_tib[i,] %>% 
    pull(speeches) %>% 
    pull_questions()
}
appearances_tib[71,] %>% 
  pull(speeches) %>% 
  pull_questions()

questions_tib <- appearances_tib %>% 
  filter(!speech_id %in% small_speeches$speech_id) %>%
  mutate(questions_dat = map(str_to_lower(speeches), pull_questions)) %>% 
  select(-speeches) %>%
  unnest() %>%
  mutate(q_id = row_number())

View(questions_tib)

q_word_vec <- c("do","what","where","who","why","when","how","if","has","will","does")

stop_words_amended <- stop_words %>% filter(!word %in% q_word_vec)

q_words <- questions_tib %>%
  mutate(q_id = row_number()) %>%
  unnest_tokens(word, question_sentence_full) %>%
  anti_join(stop_words_amended) %>%
  #filter(!word %in% my_stop_words, !str_detect(word, "[0-9]")) %>%
  count(q_id, word, sort = TRUE) %>%
  ungroup() 

# OR, remove all stop words, and model according to first Q word i marked

q_words <- questions_tib %>%
  mutate(q_id = row_number()) %>%
  unnest_tokens(word, question_sentence_full) %>%
  anti_join(stop_words) %>%
  filter(!word %in% my_stop_words, !str_detect(word, "[0-9]")) %>%
  count(q_id, word, sort = TRUE) %>%
  ungroup() 

q_dtm <- q_words %>%
  cast_dtm(q_id, word, n)

q_dtm

q_lda <- LDA(q_dtm, k = 5, control = list(seed = 1234))

q_lda

q_topics <- tidy(q_lda, matrix = "beta")

q_topics


top_q_terms <- q_topics %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_q_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

speeches_gamma <- tidy(q_lda, matrix = "gamma") %>% rename(q_id = document)

speeches_gamma 

max_gammas <- speeches_gamma %>%
  group_by(q_id) %>%
  filter(gamma == max(gamma)) %>%
  ungroup() %>%
  mutate(q_id = as.numeric(q_id))

questions_tib %>%
  inner_join(max_gammas) %>%
  mutate(first_qword = unlist(first_qword),
         first_qword = str_extract(first_qword, "[a-z]+")) %>%
  group_by(first_qword) %>%
  count(topic) %>%
  ggplot(aes(first_qword, n)) + geom_col() + facet_wrap(~topic) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


qs_anno <- questions_tib %>%
  mutate(tagged_sents = map(question_sentence_full, ~anno(., ".*"))) %>%
  select(-first_qword, all_qwords) %>%
  filter(!is.na(question_sentence_full)) 

qs_anno[1,]$tagged_sents

# FIx this

qs_anno %<>% 
  mutate(nsents = map(tagged_sents, nrow), q_ids = map2(q_id, nsents, rep))

tagged_qs <- reduce(qs_anno$tagged_sents, bind_rows) %>% add_column(q_id = unlist(qs_anno$q_ids)) 

questions_tib %>% View() 

tagged_qs %>%
  filter(str_detect(tags, "N.+")) %>%
  View()
