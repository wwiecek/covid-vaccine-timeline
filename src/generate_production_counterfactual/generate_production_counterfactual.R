# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/generate_production_counterfactual"))
# setwd(root) # Use to get back to root

# create a daily vaccine production series for a manufacturer
get_daily_production = function(prod_data) {
  final_date = as.Date("2020-12-31")

  len_1st_scale = as.numeric(
    difftime(prod_data$scale_up, prod_data$scale_up_start, unit="days"))
  len_constant = as.numeric(
    difftime(prod_data$increase_scale, prod_data$scale_up, unit="days"))
  len_2nd_scale = as.numeric(
    difftime(prod_data$full_scale_up, prod_data$increase_scale, unit="days"))
  len_final = as.numeric(
    difftime(final_date, prod_data$full_scale_up, unit="days"))

  # let x be the partial scale up daily rate, then total produced is
  # len_1st_scale*x/2 + len_constant*x + len_2nd_scale*x +
  # len_2nd_scale*(full_scale_daily_rate - x)/2 + len_final * full_scale_daily
  partial_scale_daily = round((
    prod_data$total_made_2020 - len_final * prod_data$full_scale_daily -
      len_2nd_scale * prod_data$full_scale_daily / 2
  ) / (
    len_1st_scale/2 + len_constant + len_2nd_scale - len_2nd_scale/2
  ))

  dates = seq(prod_data$scale_up_start, final_date, by = 1)
  daily_production = round(c(
    0,
    seq(0, partial_scale_daily, length.out = len_1st_scale),
    rep(partial_scale_daily, times = len_constant),
    seq(partial_scale_daily, prod_data$full_scale_daily, length.out = len_2nd_scale),
    rep(prod_data$full_scale_daily, times = len_final)
  ))

  final_df = data.frame(
    date = dates,
    daily_production = daily_production
  )

  return(final_df)
}

production = data.frame(
  vaccine = c("Pfizer", "Moderna"),
  scale_up_start = as.Date(c("2020-03-17", "2020-03-23")),
  scale_up = as.Date(c("2020-07-31", "2020-07-31")),
  increase_scale = as.Date(c("2020-11-27", "2020-12-04")),
  full_scale_up = as.Date(c("2020-12-11", "2020-12-18")), #days of approval
  full_scale_daily = c(1000000, 500000),
  total_made_2020 = c(50000000, 20000000)
)

pfizer_prod = production %>% filter(vaccine == "Pfizer")
moderna_prod = production %>% filter(vaccine == "Moderna")

pfizer_daily = get_daily_production(pfizer_prod)
moderna_daily = get_daily_production(moderna_prod)

total_production = pfizer_daily %>%
  full_join(moderna_daily, by = c("date"), suffix = c("_pfi", "_mod")) %>%
  replace(is.na(.), 0) %>%
  mutate(daily_vaccines = daily_production_pfi + daily_production_mod) %>%
  mutate(cumulative_available_vaccines = cumsum(daily_vaccines))

unconstrained_period = data.frame(
  date = seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = 1),
  cumulative_available_vaccines = Inf
)
full_production = bind_rows(total_production, unconstrained_period) %>%
  select(date, cumulative_available_vaccines)

us_prod = full_production %>%
  mutate(country = "United States", iso3c = "USA")
uk_prod = full_production %>%
  mutate(country = "United Kingdom", iso3c = "GBR")
counterfactual_production = bind_rows(us_prod, uk_prod)

saveRDS(total_production, "detail_production.Rds")
saveRDS(pfizer_daily, "pfizer_estimated_production.Rds")
saveRDS(moderna_daily, "moderna_estimated_production.Rds")
saveRDS(counterfactual_production, "counterfactual_production.Rds")
