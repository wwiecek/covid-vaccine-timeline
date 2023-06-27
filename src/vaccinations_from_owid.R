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
	 dplyr::rename(
	 	country = location,
	 	iso3c = iso_code
	 	) %>%
	 filter(iso3c %in% c("USA","GBR")) %>%
	 filter(date <= as.Date('2022-10-01')) %>%
	 select(country, iso3c, date, first_doses, second_doses, third_doses)

uk_predata_coef_primary = 2286572/sum(1:34)
uk_predata_coef_secondary = 391399/sum(1:34)

uk_extra_vaccinations <- data.frame(
	country = rep('United Kingdom',34),
	iso3c = rep('GBR',34),
	date = as.character(as.Date(as.Date('2020-12-08'):as.Date('2021-01-10'))),
	first_doses = uk_predata_coef_primary*1:34,
	second_doses = uk_predata_coef_secondary*1:34,
	third_doses = rep(0,34)
)

vaccinations <- rbind(vaccinations, uk_extra_vaccinations) %>%
	arrange(iso3c, date)	

write.csv(vaccinations,'data/raw/owid-raw.csv')
