library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidygeocoder)

#read available data
biobankGEO <- read.csv("data/BiobankGEO.csv")
census_api_key("7fb226a74fd6f4070ccb9378c3308fd07104dc51", overwrite = T, install = T)

address <- read.csv("data/EmCAB_address.csv", strip.white=TRUE)

# Cleanup as much as possible
address$state <- trimws(address$state)
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



#Generate geocode from address
address_single <- paste(address_comb$address, sep = ", ")
address_single <- tibble(singlelineaddress = address_single)
address_single$singlelineaddress <- sub("#", '', address_single$singlelineaddress)
a <- address_single %>% geocode(address = singlelineaddress, verbose=TRUE)
census_full1 <- address_single %>% geocode(
  address = singlelineaddress,
  method = "census", full_results = TRUE, api_options = list(census_return_type = 'geographies')
)
census_full2 <- census_full1[which(!is.na(census_full1$match_type)),]


#Proceed to get_acs step
#define variable of interest
atl_vars_short <- c(
  total_pop = "B01003_001",
  poverty = "B17001_002"
  #median_income = "B19013_001",
  #households = "B11001_001",
  #housing_units = "B25001_001",
  #agg_hh_income ="B19025_001"
)
# This works
all_states <- c('01','02','04','05','06','08','12','13','17','18','21','24','26','27','28','29','37','41','45')
cbg_data_atl_2013 <- get_acs(geography = "tract", 
                             variables = atl_vars_short, 
                             state = all_states,
                             year = 2010,
                             geometry = FALSE)
#calculating poverty rate
test1 <- cbind(cbg_data_atl_2013[(seq_len(nrow(cbg_data_atl_2013)) %% 2) == 1, 'GEOID'], cbg_data_atl_2013[(seq_len(nrow(cbg_data_atl_2013)) %% 2) == 1, 'estimate'], cbg_data_atl_2013[(seq_len(nrow(cbg_data_atl_2013)) %% 2) == 0, 'estimate'])
colnames(test1)[2:3] <- c('total', 'poverty')
test1$poverty_rate <- (test1$poverty / test1$total)
test1 <- test1[which(test1$total != 0),]
test1$poverty_rate <- test1$poverty_rate*100
#get as much poverty pct as possible for the 7303 people enrolled in biobank
merge_final <- merge(census_full2, test1, by.x = 'FIPS_BLOCKGROUP', by.y = 'GEOID', all.x = T)

#compare and integrate with the original povpct data in biobank, added about 120 lines of poverty information
merge_final_final <- merge(dat[c('uniqueid', 'enroldt')], biobankGEO[c('uniqueid','Pctpov')], by='uniqueid', all.x = T)
merge_final_final <- merge(merge_final_final, merge_final[c('unique_id','poverty_rate')], by.x='uniqueid', by.y='unique_id', all.x = T)
merge_final_final$integrated <- ifelse(is.na(merge_final_final$Pctpov), merge_final_final$poverty_rate, merge_final_final$Pctpov)
#c('01','02','04','05','06','08','12','13','17','18','21','24','26','27','28','29','37','41','45')

write.csv('geocoded.csv', x = merge_final_final)
