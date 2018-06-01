# Web scraping wikipedia tutorial

library(rvest)
pacman::p_load(tidyverse, magrittr, tidytext)

# https://en.wikipedia.org/wiki/James_Mackay,_Baron_Mackay_of_Clashfern


url <- "https://en.wikipedia.org/wiki/Members_of_the_House_of_Lords"

links <- url %>%
  read_html() %>%
  html_nodes("ul") %>%
  html_text(trim = TRUE) %>%
  strsplit(split = "\n") %>%
  unlist()

lord_links <- url %>% read_html() %>% html_nodes("td:nth-child(1) a")

url_prefix = "https://en.wikipedia.org"
lord_links %<>% html_attr("href")
lord_links <- map(lord_links, ~str_c(url_prefix, .))
lord_links <- keep(lord_links, ~!str_detect(., "cite_note"))

### test ###

link_clashfern <- lord_links %>% keep(~str_detect(., "Clashfern"))
link_clashfern <- link_clashfern[[1]]

link_clashfern %>% 
  read_html() %>%
  html_nodes("td:nth-child(2) a")

link_clashfern %>%
  read_html() %>%
  html_nodes("tr:nth-child(35) th a")

link_clashfern %>%
  read_html() %>%
  html_nodes("th .nowrap")

infobox <- link_clashfern %>%
  read_html() %>%
  html_nodes(".vcard") %>%
  html_text("infobox vcard")

# function: process vcard/infobox

infobox <- infobox[1]
infobox %>% str_view_all("\\n")

# start wiht just the personal details

infobox <- str_replace_all(infobox, "\\n", "_")
str_view_all(infobox, "Personal details.*$")
personal_box <- str_extract(infobox, "(?<=Personal details_).*$")
personal_box %<>% str_split("_") %>% unlist()

pbox_to_tibble <- function(pbox) {
  tibble(category = c(pbox[1], pbox[3], pbox[5]), value = c(pbox[2], pbox[4], pbox[6]))
}
pbox_to_tibble(personal_box)

getInfoboxFromLink <- function(link) {
  link %>%
    read_html() %>%
    html_nodes(".vcard") %>%
    html_text("infobox vcard")
}

# first ten are parties, can use nlp to detect names later blah
lord_links %>% head(20)
lord_links <- lord_links[11:length(lord_links)]


getInfoboxFromLink <- function(link) {
  link %>%
    read_html() %>%
    html_nodes(".vcard") %>%
    html_text("infobox vcard")
}

clean_infobox <- function(infobox) {
  infobox <- infobox[1]
  
  # start wiht just the personal details
  
  infobox <- str_replace_all(infobox, "\\n", "_")
  personal_box <- str_extract(infobox, "(?<=Personal details_).*$")
  personal_box %<>% str_split("_") %>% unlist()
}

infoboxes <- lord_links[50:100] %>% map(getInfoboxFromLink)
infoboxes_clean <- map(infoboxes, clean_infobox)

pbox_to_tibble <- function(pbox) {
  dob = which(pbox == "Born")
  party = which(pbox == "Political party")
  almamater = which(pbox == "Alma mater")
  tibble(category = c(pbox[dob], pbox[party], pbox[almamater]), value = c(pbox[dob+1], pbox[party+1], pbox[almamater+1]))
}
infoboxes_clean %>% pbox_to_tibble()

# shitty way of getting name
names_pulled <- lord_links[50:100] %>% map(str_extract, "/[A-Z].*$")
info_tibbles <- map(infoboxes_clean, pbox_to_tibble)

# add names to tibbles
info_tibbles <- map2(info_tibbles, names_pulled, ~add_column(.x, name = .y) %>% select(name, everything()))
info_tibbles %<>% reduce(bind_rows)

info_tibbles %>% View()
info_tibbles %>% filter(category == "Alma mater") %>% View()

# processing strings

born_str <- info_tibbles %>% filter(category == "Born") %>% pull(value) %>% `[[`(1)

# extract birth name (all chars up until first bracket)
born_str %>% str_extract("^[^(]*")

# date and age, not exactly specific but it works
datage <- born_str %>% str_extract_all("\\([^()]*\\)")
date <- datage[[1]][1]
age <- datage[[1]][2]
date
age

# getting the info tables 

tables <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

lords <- tables[[4]][-3]
lords_spiritual <- tables[[3]]
# leave of absence
loa <- tables[[5]][-2]
dead <- tables[[8]][-2]

saveRDS(lords, "data//lords_table.RDAT")
saveRDS(lords_spiritual, "data//lords_spiritual_table.RDAT")


# pull text from education and see which mention prep schools
# https://en.wikipedia.org/wiki/List_of_boarding_schools_in_the_United_Kingdom
# https://en.wikipedia.org/wiki/List_of_independent_schools_in_England

# What else is interesting to scrape that might lend to my analysis?