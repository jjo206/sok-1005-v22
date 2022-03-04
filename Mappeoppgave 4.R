library(tidyverse)
library(purrr)
library(rvest)
library(rlist)

Fag <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list",
            "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list",
            "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list")


#--- Lager funksjon

Funksjon <- function(Fag){
  page <- read_html(Fag)
  table <- html_nodes(page, 'table')
  table <- html_table(table, fill=TRUE)
  dframe <- list.stack(table)
  colnames(dframe) <- dframe[1,]
  dframe <- dframe %>% filter(!Dato=="Dato")
  dframe <- dframe %>% separate(Dato, 
                                into = c("Dag", "Dato"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])")
  dframe$Dato <- as.Date(dframe$Dato, format="%d.%m.%Y")
  dframe$Uke <- strftime(dframe$Dato, format = "%V")
  dframe <- dframe %>% select(Dag, Dato, Uke, Tid, Rom, Emnekode)
  return(dframe)
}

Timeplan <- map(Fag, Funksjon)

Timeplan <- bind_rows(Timeplan)
