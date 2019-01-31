# Text Mining with R #

#### load ####
pacman::p_load(tidytext, tidyverse, stringr, janeaustenr, gutenbergr, magrittr)

#### Intro ####

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

text_df <- tibble(line = 1:4, text)
text_df

# Tokenization is the process of splitting text up into usable units - "tokens". 
text_df %>%
  unnest_tokens(word, text)

# similar to...
str_extract_all(text, "(\\w*\\w)")

# Jane Austen 
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
original_books

# To tokenize this data, we use unnest_tokens() again...
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

# We can remove stop words (common words) from the analysis. 
# Stop words are kept in a df- so we can match these words using anti_join()
stop_words

data(stop_words)
stop_words %>% semi_join(tidy_books) # all the stop words that appear.
tidy_books <- tidy_books %>%
  anti_join(stop_words) # boom, gone. 

# This keeps observations from tidy_books (1st argument) that weren't matched. 

# Using count() to summarise the most common values
tidy_books %>%
  count(word, sort = TRUE)

# We can 
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#### Working with gutenbergr ####

# These are the codes for island of dr moreau, the time machine, WotW + the invisible man
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Tokenization and removal of stop words 
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Which words does our pal HG Wells use most?
tidy_hgwells %>%
  count(word, sort = TRUE)

# And the same for the Bronte sisters...
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

# Now lets bind all this frequency data from different authors together

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen"))

frequency %<>% mutate(word = str_extract(word, "[a-z']+")) %>% # extract words
  count(author, word) %>% # silently group by author and count word frequency
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% # proportional use of each word
  select(-n) %>% # remove absolute count
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

# To do a comparison against jane austen, we made her into a variable by spreading
# then only  gathering together bronte and wells back into variables

library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


# Let's compare similarities more rigorously using correlation tests

cor.test(data = filter(frequency, author == "Brontë Sisters"), 
         ~ proportion + `Jane Austen`)

cor.test(data = filter(frequency, author == "H.G. Wells"), 
         ~ proportion + `Jane Austen`)


#### Sentiment Analysis ####
sentiments

get_sentiments("nrc")
get_sentiments("bing")

# convert to tidy text format
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# filter the nrc lexicon for all the "joy" words
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# filter for the "Emma" book and match to the joy lexicon- count which joy words
# are most frequent. 
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# How does sentiment change across the entire text?
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Visualise the sentiment change for each book
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# Doing the same for hgwells #

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

wells_books <- tibble(book = c("the time machine", "the war of the worlds", "the invisible man", "the island of docotr moreau"),
       gutenberg_id = c(35, 36, 5230, 159))

hgwells_tidy <- hgwells %>% left_join(wells_books) %>% group_by(book) %>%
  mutate(linenumber = row_number()) %>% ungroup() %>% unnest_tokens(word, text)  
hgwells_tidy %<>% anti_join(stop_words)

get_sentiments("bing")

hgwellssentiment <- hgwells_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative, negative = -negative)

hgwellssentiment_ns <- hgwells_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(negative = -negative) %>%
  gather(positive, negative, key = sentiment, value = score, -book, -index)

ggplot(hgwellssentiment_ns, aes(index, score, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# Pretty negative guy on the whole...


# Now let's compare each sentiment lexicon against one another # 

# How do they each define how the sentiment changes throughout pride and prejudice
pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")
pride_prejudice

# For afinn we can simply sum the scores assigned for each word within each index
afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

afinn

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Bing in general has a lower variance in scores than nrc even though they use
# the same modifier - what could be biasing "lower" scores to appear?

get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive",
                          "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

# In bing we do indeed see a higher ratio of negative:positive words.

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Wordcloud package uses base R # 

library(wordcloud)
hgwells_tidy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) #%>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


#### Term Frequency Analysis ####

# A heuristic approach to determine which words are important in the text, by
# weighting words depending on their frequency within a set of documents. A
# word that occurs often will be assigned a small weight factor. Then the 
# frequency of words IN EACH document is multiplied by this factor, to get an
# idea of how important that word was in the particular document. 

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% group_by(book) %>% summarise(total = sum(n))

book_words %<>% left_join(total_words)
book_words

# we can calculate term frequency by dividing n by the total number of words

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE, bins = 50) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# examining Zipfs law

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)
how_zipfy <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = how_zipfy[[1]][[1]], slope = how_zipfy[[1]][[2]], color = "gray50", linetype = 2) +
  geom_abline(intercept = max(log10(freq_by_rank$`term frequency`)), slope = -1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# This model defines the distribution of the corpus well for most words. compared
# to this model, higher rank (more common) words are less common than expected by a power law.
# and so are lower rank (less common) words. 

# bind_tf_idf will take a list of words separated by book and counts of each word
# within each document, and tells us the tf_idf- a heuristic of how prominent a word
# is in a document compared to the whole corpus.

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

# which words are important to each document. note: idf = ln(n_doc/n_corpus)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

#### TF-IDF analysis of a physics corpus ####

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

physics_words <- physics %>% 
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

plot_physics <- physics_words %>% 
  bind_tf_idf(word, author, n) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# What is this "eq" term that arises in the einstein text

physics %>%
  filter(str_detect(text, "eq\\W")) %>%
  select(text)

# It seems to be a label for images. 
# Whereas K1 is a specific term for coordinate systems

physics %>%
  filter(str_detect(text, "K1\\W")) %>%
  select(text)

# We can make a custom list of stop words

mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn",
                                   "fig", "file", "cg", "cb", "cm"))

physics_words %<>% anti_join(mystopwords)

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()


#### Tokenizing by ngram ####

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_books()

austen_bigrams %>% count(bigram, sort = TRUE)

# Many of the bigrams include stop words. We could filter these out to get at 
# the interesting stuff...

# Use separate to get each word in the bigram as a different variable
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Remove observations where word1 or word2 in a bigram is a stop word
bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# same as
bigrams_filtered_AND <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)

#  if we want to keep cases where only one of the words is a stop word
bigrams_filtered_OR <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word | !word2 %in% stop_words$word)

bigrams_filtered_AND %>% unite(word1, word2, col = bigram, sep = " ") %>%
  count(bigram, sort = TRUE)

bigrams_filtered_OR %>% unite(word1, word2, col = bigram, sep = " ") %>%
  count(bigram, sort = TRUE)

# or keep separated 
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# We can do a similar analysis for trigrams

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
