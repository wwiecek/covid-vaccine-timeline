require(zoo)
require(dplyr)

pad_diff <- function(vec) {
	return(diff(c(0,vec)))
}

replace_na <- function(vec) {
	return(replace(vec,is.na(vec),0))
}

vaccinations <- read.csv('data/raw/owid_vaccinations.csv') %>%
	 mutate(total_boosters = replace_na(total_boosters)) %>% 
	 mutate(first_doses = replace_na(daily_people_vaccinated)) %>%
	 group_by(iso_code) %>%
	 mutate(second_doses = pad_diff(rollmean(people_fully_vaccinated,k=5,fill=0))) %>%
	 mutate(third_doses = pad_diff(rollmean(total_boosters,k=5,fill=0))) %>%
	 mutate(third_doses = replace(third_doses,c(1,2,3),0)) %>%
     mutate(second_doses = replace(second_doses,c(1,2,3),0)) %>%
	 rename(country = location) %>%
	 rename(iso3c = iso_code) %>%
	 filter(iso3c %in% c("USA","GBR")) %>%
	 filter(date <= as.Date('2022-10-01')) %>%
	 select(country, iso3c, date, first_doses, second_doses, third_doses)

write.csv(vaccinations,'data/raw/counterfactual_timelines/owid-raw.csv')