### Analysing Trump's tweets ###

pacman::p_load(purrr,dplyr,tibble,stringr)

#load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
glimpse(trump_tweets_df)

tweets <- trump_tweets_df$text
tweets %>% head() %>% strtrim(70)
regex <- "badly|crazy|weak|spent|strong|dumb|joke|guns|funny|dead"

str_subset(tweets, regex)

#scaling down the dataset

tweets <- tweets[c(1, 2, 5, 6, 198, 347, 919)]
tweets %>% strtrim(70)

# The base R function gregexpr matches regex patterns in strings. returns a list
matches <- gregexpr(regex, tweets)
str(matches)
?gregexpr # apparently -1 means no match and other numbers are start positions of
# matches. another attribute holds the length of each of the matches (in chrs)

matches[[4]]
?substring

# Ultimately what we want to do is feed three lists: one of text, one of the first
# char's position, and one of the last char's match position into substring
# element-by-element.

matches
map_int(matches, length) # how many numbers are stored in each vector of matches list

# You can extract attributes using the attr() function

m <- matches[[7]]
attr(m, which = "match.length")

# Now apply this approach to the rest of the list

map(matches, attr, which = "match.length") # or
m1 <- function(x) attr(x, which = "match.length")
map(matches, m1) # or even
match_length <- map(matches, ~ attr(.x, which = "match.length"))

nomatch <- matches[[1]]
yesmatch <- matches[[7]]

num_matches <- function(x) ifelse(m1(x) == -1, return(0), return(length(x)))

map(matches, num_matches)
# or
map(matches, ~ sum(.x > 0))
# or return a vector of number of matches
map_dbl(matches, num_matches)

# Now we have the number of matches in each tweet

# Now we want to get the values of the first character position ofeach match
(match_first <- map(matches, as.vector))
#as.vector strips attributes

# Now we have a vector of match lengths, and a vector of positions of matches...

#let's try this out on a subset of the data

test_first_pos <- match_first[[7]]
test_length <- match_length[[7]]
test_last_pos <- test_first_pos+test_length-1
substring(tweets[[7]], test_first_pos, test_last_pos)

# This code should also return nothing for tweet[[1]]

test_first_pos <- match_first[[1]]
test_length <- match_length[[1]]
test_last_pos <- test_first_pos+test_length-1
substring(tweets[[1]], test_first_pos, test_last_pos)

# make a list of the last matches 

match_last <- map2(match_first, match_length, ~ .x+.y-1)
match_last

# pmap()    allows you to input different lists of arguments, element by element
# into a function

# store your lists of arguments into one big list

# Arguments must be named (in names of lists)
input_list = list(text = tweets, first = match_first, last = match_last)
input_list
pmap(input_list, substring)

# we can also apply pmap across variables in a tibble

mdf <- tibble(
  text = tweets,
  first = match_first,
  last = match_last
)
mdf

words <- mdf %>% pmap(substring) 
mutate(mdf, words = words) %>% View()

# all in one

tibble(text = tweets,
       first = gregexpr(regex, tweets)) %>% 
  mutate(match_length = map(first, ~ attr(.x, which = "match.length")),
         last = map2(first, match_length, ~ .x + .y - 1)) %>%
  select(-match_length) %>% 
  pmap(substring)

