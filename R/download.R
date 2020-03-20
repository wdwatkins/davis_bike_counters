# scrape links from webpage
#download all showdocument links
#name appropriately
library(rvest)
library(dplyr)
data_page_html <- read_html('https://www.cityofdavis.org/city-hall/public-works-engineering-and-transportation/bike-pedestrian-program/bike-and-pedestrian-data-statistics')
li_elements <- xml_find_all(data_page_html, './/li')
li_content <- xml_contents(xml_children(li_elements))
lapply(li_content, bind_rows)
li_text <- html_text(li_elements) %>% grep(pattern = "counts", value = TRUE)
li_href <- html_attr(html_children(li_elements), "href") %>%
  grep(pattern = 'showdocument', value = TRUE, ignore.case = TRUE) %>% 
  unique() #one link is duplicated in the html, this removes it

link_df <- tibble(names = li_text, link = li_href) %>% 
  mutate(link = if_else(condition = grepl(pattern = "cityofdavis", x = link),
                        true = link,
                        false = paste0('https://cityofdavis.org', link)),
         names = paste0(names, ".xlsx"))

for(i in seq_along(link_df$names)) {
  download.file(url = link_df$link[i], destfile = file.path('in', link_df$names[i]))
}
