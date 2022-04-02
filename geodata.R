library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)

census_api_key("7fb226a74fd6f4070ccb9378c3308fd07104dc51", overwrite = T)

address <- read.csv("data/EmCAB_address.csv",strip.white=TRUE)

address$state <-trimws(address$state)
address$state[which(toupper(address$state)=="FI")] <- "FL"
address$state[which(toupper(address$state)=="FA")] <- "GA"
address$state[which(tolower(address$state)=="gergia")] <- "GA"
address$state[which(tolower(address$state)%in%c("g", "ga."))] <- "GA"
address$state[grep(address$state, pattern = "-", fixed = T)] <- 
  gsub(" ", "",sapply(strsplit(address$state[grep(address$state, pattern = "-", fixed = T)], "-", fixed = T), "[[", 1))
address$zip[which(address$zip %in% c("20003", "20839", "25909"))] <- c('30004','30345','28909')
uszips <- read.csv("data/uszips.csv")
uszips <- uszips[c("zip", "city", "state_id", "state_name")]
address_comb <- merge(address, uszips, by="zip")
error_terms <- address_comb[which((toupper(address_comb$state)!=address_comb$state_id) & (tolower(address_comb$state)!=tolower(address_comb$state_name))),]

#get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),state = "GA", zcta = "30329")

median_income<-get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),
                       state = address_comb$state_id[1:1500], 
                       zcta = address_comb$zip[1:1500])
median_income<-rbind(median_income, get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),
                                            state = address_comb$state_id[1501:3000], 
                                            zcta = address_comb$zip[1501:3000]))
median_income<-rbind(median_income, get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),
                                            state = address_comb$state_id[3001:4500], 
                                            zcta = address_comb$zip[3001:4500]))
median_income<-rbind(median_income, get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),
                                            state = address_comb$state_id[4501:6000], 
                                            zcta = address_comb$zip[4501:6000]))
median_income<-rbind(median_income, get_acs(geography = "zcta", variables = c(medincome = "B19013_001"),
                                            state = address_comb$state_id[6001:7303], 
                                            zcta = address_comb$zip[6001:7303]))
colnames(median_income) <- c("zip","NAME","variable","Income","IncomeErrorMargin")
median_income <- median_income[c("zip","Income","IncomeErrorMargin")]

address_income <- merge(address_comb, median_income, by='zip')
address_income <- address_income[which(!duplicated(address_income)),]
