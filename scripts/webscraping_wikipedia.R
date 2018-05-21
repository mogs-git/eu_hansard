# Web scraping wikipedia tutorial

library(rvest)
pacman::p_load(tidyverse, magrittr)

https://en.wikipedia.org/wiki/James_Mackay,_Baron_Mackay_of_Clashfern


url <- "https://en.wikipedia.org/wiki/Timeline_of_wars"
periods <- url %>%
  read_html() %>%
  html_nodes("ul") %>%
  html_text(trim = TRUE) %>%
  strsplit(split = "\n") %>%
  unlist()
periods <- periods[18:26]
periods[1] <- tolower(gsub(" ", "_", periods[1]))
periods

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

link_clashfern %>%
  read_html() %>%
  html_nodes(".vcard") %>%
  html_text("infobox vcard")

# function: process vcard.