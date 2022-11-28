usa_raw_data = read.csv("COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")

usa_distributed_vaccines_by_state = usa_raw_data %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(date, Location, Distributed)

usa_available_vaccines_series = usa_distributed_vaccines_by_state %>%
  group_by(date) %>%
  summarise(cumulative_available_vaccines = sum(Distributed)) %>%
  # before this date the observations are daily, afterwards weekly
  # keeping the daily section
  filter(date < "2022-06-22") %>%
  mutate(country = "United States", iso3c = "USA")

# mock UK data for now, with same length as US data
uk_data = data.frame(
  date = seq(as.Date("2020-12-13"), as.Date("2022-06-16"), by="days"),
  cumulative_available_vaccines = 0,
  country = "United Kingdom",
  iso3c = "GBR"
)

baseline_vaccine_production = bind_rows(usa_available_vaccines_series, uk_data)

saveRDS(baseline_vaccine_production, "baseline_vaccine_production.Rds")
