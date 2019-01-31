
main_parties <- appearances_tib %>%
  count(party) %>% 
  arrange(desc(n)) %>%
  head(4) %>%
  pull(party)
  
get_quartiles <- function(df) {
  nr <- df %>% nrow(.)
  nr <- c(1:4) * nr/4 
  nr %<>% round(0)
  dnr <- diff(nr)
  section <- c(rep("1st", nr[1]), map2(c("2nd", "3rd", "4th"), dnr, rep)) %>% unlist()
}
sections <- get_quartiles(appearances_tib)
mycols = c("blue4", "red3", "chartreuse4", "gold1", "grey")
levels(appearances_tib$party_f)
appearances_tib$party_f <- fct_relevel(appearances_tib$party_f, main_parties)

### output ####
engagement_per_quarter <- appearances_tib %>% 
  mutate(sec = sections) %>% 
  group_by(sec, party_f) %>% 
  count() %>%
  ggplot(aes(party_f, n)) +
  geom_col(aes(fill = party_f)) +
  facet_wrap(~sec) + 
  scale_fill_manual(values = mycols)

engagement_per_quarter


# Number of times two lords from the same party speak consequtively. 

appearances_tib %>%
  mutate(next_party = lead(party),
         match = party == next_party) %>%
  filter(match) %>%
  count(party)


# Lords who made the most speeches

speech_counts <- appearances_tib %>%
  group_by(name) %>%
  summarise(n_speeches = n(), party = unique(party_f)) %>%
  arrange(desc(n_speeches))

speech_counts %>%
  group_by(n_speeches, party) %>%
  summarise(count = n()) %>%
  ggplot(aes(n_speeches, count)) +
  geom_col(aes(fill = party)) + 
  scale_fill_manual(values = mycols) +
  scale_x_continuous(breaks = c(1:12)) + 
  scale_y_continuous(breaks = seq(0,40,5))

speech_counts %>%
  filter(party %in% c("(Con)", "(Lab)", "(LD)", "(CB)")) %>%
  ggplot(aes(n_speeches)) + stat_ecdf(aes(colour = party)) + scale_colour_manual(values=mycols)

speech_counts %>% filter(n_speeches > 3) %>% count(party) %>% mutate(tot = sum(n), p  = n/tot)
speech_counts %>% group_by(party) %>% summarise(n = sum(n_speeches)) %>% ungroup() %>% mutate(tot = sum(n), p = n/tot)

speech_counts  %>%  group_by(party) %>% summarise(median_sp = median(n_speeches))

speech_props <- speech_counts %>%
  group_by(n_speeches, party) %>%
  summarise(n_per_party = n()) %>%
  group_by(n_speeches) %>%
  mutate(tot = sum(n_per_party), p = n_per_party/tot)

speech_props %>%
  mutate(top = p == max(p)) %>%
  filter(top) %>%
  ggplot(aes(n_speeches, p, fill = party)) + geom_col(position = "dodge")  + scale_fill_manual(values = mycols)
  
speech_props %>% 
  ggplot(aes(n_speeches, p, colour = party)) + 
  geom_point() +
  geom_line() + 
  scale_colour_manual(values = mycols) 

speech_counts %>%
  filter(n_speeches > 1) %>%
  group_by(party) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(tot = sum(n), p = n/tot)

