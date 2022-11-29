# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/get_baseline_production"))
# setwd(root) # Use to get back to root

usa_raw_data = read.csv("COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")

usa_distributed_vaccines_by_state = usa_raw_data %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(date, Location, Distributed, Administered)

usa_available_vaccines_series = usa_distributed_vaccines_by_state %>%
  group_by(date) %>%
  summarise(cumulative_available_vaccines = sum(Distributed),
            cumulative_administered_vaccines = sum(Administered)) %>%
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

baseline_vaccine_production = bind_rows(usa_available_vaccines_series, uk_data) %>%
  select(date, cumulative_available_vaccines, country, iso3c)

saveRDS(baseline_vaccine_production, "baseline_vaccine_production.Rds")

# Plot how distributed amount of vaccines compared to administered changes over time
# for the US data
distributed_vs_administered = ggplot(data = usa_available_vaccines_series) +
  geom_line(aes(x = date, y = cumulative_available_vaccines, color = "Distributed")) +
  geom_line(aes(x = date, y = cumulative_administered_vaccines, color = "Administered")) +
  ylab("vaccines distributed / available") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

ggsave("CDC_distributed_vs_administered.pdf", distributed_vs_administered, device = "pdf")
