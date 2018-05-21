# google search data using gsearch

devtools::install_github('PMassicotte/gtrendsR')
install.packages('curl')
pacman::p_load(gtrendsR,maps,ggplot2,lettercase, magrittr, tidyverse)

gbrex <- gtrendsR::gtrends(c("leave", "remain"), time="all", gprop = "web", geo = c("GB"))
gtrendsR::countries %>% filter(name  == "UNITED KINGDOM")

plot(gbrex)
gbrex %>% attributes()

gbrex$interest_over_time %>% head()
map(gbrex, head)
gbrex$interest_over_time %>% mutate(hits = as.numeric(hits)) %>% ggplot(aes(date, hits)) + geom_line(aes(colour = keyword))
gbrex$interest_by_city
gbrex$related_queries

# take all brexit related searches

# take all dates by month
dates <- gbrex$interest_over_time$date
city_involvment <- list()
# only use dates starting from jan first 2015
for (i in 306:(length(dates)-1)) {
  city_involvment[[i]] <- gtrends(c("leave", "remain"), time = str_c(dates[i], dates[i+1], sep = " "), gprop = "web", geo = c("GB")) %>% `$`(interest_by_city)
}


city_involvment[[1]]
city_tib <- tibble(date = dates[318:(length(dates)-1)], city = city_involvment[318:(length(dates)-1)])
city_tib <- unnest(city_tib)
city_tib %>% group_by(location) %>% count(sort = T) 


similar_terms <- list()
for (i in 306:(length(dates)-1)) {
  similar_terms[[i]] <- gtrends(c("leave", "remain"), time = str_c(dates[i], dates[i+1], sep = " "), gprop = "web", geo = c("GB")) %>% `$`(related_queries)
}

terms <- map(similar_terms, "value")
term_tib <- tibble(date = dates[1:(length(dates)-1)], terms)
term_tib %>% mutate(term_is_null = map_lgl(terms, is.null)) %>% filter(!term_is_null) %>% unnest() %>% View()
