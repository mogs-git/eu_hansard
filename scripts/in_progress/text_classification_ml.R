#### Text Classification

library(tidyverse)
library(gutenbergr)

titles <- c(
  "The War of the Worlds",
  "Pride and Prejudice"
)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title") %>%
  mutate(document = row_number())

books

# Train a model that takes an individual line and tells us whether it came from
# War of the worlds or Pride and Prejudice

library(tidytext)

tidy_books <- books %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tidy_books


# Example plot of the top 20 most used words in each text

tidy_books %>%
  count(title, word, sort = TRUE) %>%
  anti_join(get_stopwords()) %>%
  group_by(title) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(word, n), n,
             fill = title
  )) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~title, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    subtitle = "Words like 'said' occupy similar ranks but other words are quite different"
  )


# Split data into training and test

library(rsample)

books_split <- books %>%
  select(document) %>%
  initial_split()
train_data <- training(books_split)
test_data <- testing(books_split)

# convert to sparse matrix

sparse_words <- tidy_books %>%
  count(document, word) %>%
  inner_join(train_data) %>% # only documents in training data remain
  cast_sparse(document, word, n)

class(sparse_words)

dim(sparse_words)

word_rownames <- as.integer(rownames(sparse_words))

books_joined <- data_frame(document = word_rownames) %>%
  left_join(books %>%
              select(document, title))

books_joined


library(glmnet)
library(doMC)
registerDoMC(cores = 2)

is_jane <- books_joined$title == "Pride and Prejudice"
model <- cv.glmnet(sparse_words, is_jane,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)

plot(model)
plot(model$glmnet.fit)

library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most",
    subtitle = "A document mentioning Martians is unlikely to be written by Jane Austen"
  )


intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

classifications <- tidy_books %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(document) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))

classifications


library(yardstick)

comment_classes <- classifications %>%
  left_join(books %>%
              select(title, document), by = "document") %>%
  mutate(title = as.factor(title))

comment_classes %>%
  roc_curve(title, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "midnightblue",
    size = 1.5
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) +
  labs(
    title = "ROC curve for text classification using regularized regression",
    subtitle = "Predicting whether text was written by Jane Austen or H.G. Wells"
  )

comment_classes %>%
  roc_auc(title, probability)

comment_classes %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "Pride and Prejudice",
      TRUE ~ "The War of the Worlds"
    ),
    prediction = as.factor(prediction)
  ) %>%
  conf_mat(title, prediction)

comment_classes %>%
  filter(
    probability > .8,
    title == "The War of the Worlds"
  ) %>%
  sample_n(10) %>%
  inner_join(books %>%
               select(document, text)) %>%
  select(probability, text)

comment_classes %>%
  filter(
    probability < .3,
    title == "Pride and Prejudice"
  ) %>%
  sample_n(10) %>%
  inner_join(books %>%
               select(document, text)) %>%
  select(probability, text)
