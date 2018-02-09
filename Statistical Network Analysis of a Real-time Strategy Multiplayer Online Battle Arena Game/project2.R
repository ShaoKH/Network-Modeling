# STATS 218 Final Project

library(rvest)
library(XML)
library(readxl)
library(readr)
library(xlsx)
setwd("~/Desktop/218/Final Project")


# heros_attr <- read_excel("heros_attr.xlsx")
# hero_names2 <- gsub(" ", "-", tolower(heros_attr$Hero))
# hero_names <- gsub("'", "", hero_names2)
# hero_names

########### scrape hero matchups data from Dotabuff
data_NBA <- NULL

for (i in years) {
  print(i)
  # url <- paste("http://www.basketball-reference.com/draft/NBA_", i, ".html", sep="")
  url <- "https://www.basketball-reference.com/teams/ATL/head2head.html"
  page <- htmlTreeParse(readLines(url), useInternalNodes=T)
  table <- readHTMLTable(page)$head2head
  table$team <- i
  data <- rbind(data, table)
}
data_dotabuff