# Web scraping wikipedia tutorial

library(rvest)
pacman::p_load(tidyverse, magrittr)

https://en.wikipedia.org/wiki/James_Mackay,_Baron_Mackay_of_Clashfern


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

# pull first paragraph. 


getParaFromLink <- function(link) {
  link %>%
    read_html() %>%
    html_nodes("p:nth-child(2), p:nth-child(3)") %>%
    html_text()
}
paras <- map(lord_links[50:60], getParaFromLink)
paras[[1]]

parties <- c("Labour", "Conservative", "Liberal", "Crossbench")

getLordsSpiritual <- function(link= "https://en.wikipedia.org/wiki/Members_of_the_House_of_Lords") {
  bishops <- link %>%
    read_html() %>%
    html_nodes(":nth-child(9) td:nth-child(1) a") %>%
    html_text() 
  bishops <- bishops[str_detect(bishops, "Bishop|bishop")]
  bishops
}

getLordsSpiritual()

getLordsNames <- function(link = "https://en.wikipedia.org/wiki/Members_of_the_House_of_Lords"){
  lords <- link %>%
    read_html() %>%
    html_nodes("td:nth-child(1) a") %>%
    html_text()
  lords <- lords[!str_detect(lords, "[0-9]")]
  lords
}

getLordsNames()

getLordsInfo <- function(link = "https://en.wikipedia.org/wiki/Members_of_the_House_of_Lords", xp){
  print(xp)
  print(typeof(xp))
  lordsParties <- link %>%
    read_html() %>%
    html_nodes(xp) %>%
    html_text()
  lordsParties <- lordsParties[!str_detect(lordsParties, "[0-9]")]
  lordsParties
}
all_name_xpaths <- str_c("tr:nth-child(", 1:754, ") td")
map(getLordsInfo, all_name_xpaths)

getLordsInfo(xp = ".jquery-tablesorter tr:nth-child(1) td:nth-child(1)")

tables <- "https://en.wikipedia.org/wiki/Members_of_the_House_of_Lords" %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

tables[[3]] %>% View()
tables[[4]] %>% View()

getLordsParties()
getLordsNames()
# pull text from education and see which mention prep schools
# https://en.wikipedia.org/wiki/List_of_boarding_schools_in_the_United_Kingdom
# https://en.wikipedia.org/wiki/List_of_independent_schools_in_England

# What else is interesting to scrape that might lend to my analysis?