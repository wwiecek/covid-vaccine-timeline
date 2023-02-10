# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/generate_vaccine_counterfactual"))
# setwd(root) # Use to get back to root

#' Generate a counterfactual vaccination scenario
#' @param base_series the base real world vaccination series
#' @param prod_series tracks how many vaccines are available, which constrains
#' vaccination
#' @param shift_days how many days into the past should it move vaccination
gen_cfact_with_prod = function(base_series, prod_series, shift_days) {
  dose2_delay = 21 # how many days after first shot can a person get second

  # contains the basic interest for vaccination with vaccines available sooner
  # combined with how many vaccines are available until each day
  base_shifted_with_prod = base_series %>%
    mutate(date = as.Date(date) - shift_days) %>%
    left_join(prod_series) %>%
    replace(is.na(.), 0) %>%
    mutate(first_doses_cfact = 0,
           second_doses_cfact = 0,
           both_doses = first_doses + second_doses)

  total_vaccinations = sum(base_shifted_with_prod$both_doses)
  distribution_rates = base_shifted_with_prod %>%
    select(both_doses) %>%
    mutate(share_vacc = lag(cumsum(both_doses / total_vaccinations)))

  # keep track of how many first doses were administered so far, second shots get
  # reserved when someone comes for the first vaccination
  cumulative_vac = 0
  for (i in 1:nrow(base_shifted_with_prod)) {
    doses_cap = base_shifted_with_prod$cumulative_available_vaccines[i] - cumulative_vac
    share_vaccinated = cumulative_vac / total_vaccinations
    distribution_cap =
      distribution_rates[which.min(abs(distribution_rates$share_vacc - share_vaccinated)),]$both_doses
    distribution_cap = max(1, distribution_cap)

    available_vac = floor(min(doses_cap, distribution_cap) / 2)

    dose1_vaccinated = available_vac

    dose2_vaccinated = 0
    if (i > dose2_delay) {
      dose2_vaccinated = base_shifted_with_prod$first_doses_cfact[i - dose2_delay]
    }

    base_shifted_with_prod$second_doses_cfact[i] = dose2_vaccinated
    base_shifted_with_prod$first_doses_cfact[i] = dose1_vaccinated

    cumulative_vac = cumulative_vac + dose1_vaccinated * 2
  }

  # third doses stay the same (just shifted in time) since they most probably
  # don't face production or distribution constraints
  cfact_df = base_shifted_with_prod %>%
    mutate(first_doses = first_doses_cfact,
           second_doses = second_doses_cfact) %>%
    mutate(total_vacc = cumsum(first_doses + second_doses + third_doses)) %>%
    select(country, iso3c, date, first_doses, second_doses, third_doses, total_vacc)

  return(cfact_df)
}

# Plot counterfactual vs baseline administration of 1st and 2nd doses
# Plot how the vaccination evolves with the production cap
plot_cfact = function(cfact, baseline, production) {
  dir.create("counterfactual_plots")
  countries = unique(cfact$iso3c)

  walk(countries, function(country_iso) {
    country_cfact = cfact %>% filter(iso3c == country_iso) %>%
      mutate(total_vacc = cumsum(first_doses + second_doses + third_doses))
    country_base = baseline %>% filter(iso3c == country_iso) %>%
      mutate(total_vacc = cumsum(first_doses + second_doses + third_doses))
    country_prod = production %>% filter(iso3c == country_iso)

    plot_dose1 = ggplot() +
      geom_line(data = country_base,
                aes(x = as.Date(date), y = first_doses, color = "Baseline")) +
      geom_line(data = country_cfact,
                aes(x = as.Date(date), y = first_doses, color = "Counterfactual")) +
      xlab("Date") +
      ylab("First doses administered") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      labs(color=country_iso)

    plot_dose2 = ggplot() +
      geom_line(data = country_base,
                aes(x = as.Date(date), y = second_doses, color = "Baseline")) +
      geom_line(data = country_cfact,
                aes(x = as.Date(date), y = second_doses, color = "Counterfactual")) +
      xlab("Date") +
      ylab("Second doses administered") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      labs(color=country_iso)

    plot_total_dose = ggplot() +
      geom_line(data = country_base,
                aes(x = as.Date(date), y = total_vacc, color = "Baseline")) +
      geom_line(data = country_cfact,
                aes(x = as.Date(date), y = total_vacc, color = "Counterfactual")) +
      xlab("Date") +
      ylab("Total doses administered") +
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
      labs(color=country_iso)

    plot_prod_vs_vacc = ggplot() +
      geom_line(data = country_prod,
                aes(x = as.Date(date),
                    y = cumulative_available_vaccines,
                    color = "Production")) +
      geom_line(data = country_cfact,
                aes(x = as.Date(date), y = total_vacc, color = "Vaccination")) +
      xlab("Date") +
      ylab("Cumulative total vaccines") +
      scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9)) +
      labs(color=country_iso)


    combined_plot =
      ggarrange(plot_dose1,
                plot_dose2,
                plot_total_dose,
                plot_prod_vs_vacc,
                nrow = 2,
                ncol = 2)
    shift = cfact$shifted_by[1]
    combined_plot = annotate_figure(combined_plot, top = text_grob(
      paste0("Counterfactual with vaccines shifted by ", shift, " days."),
      color = "black",
      size = 16
    ))

    ggsave(paste0("counterfactual_plots/", country_iso, "_", shift, ".png"),
           plot = combined_plot)
  })
}

# read in real world vaccination series
base_vaccination = read.csv("owid-raw.csv") %>%
  # Cutoff later data that looks unreasonable (negative number and zeros)
  filter(date < as.Date("2022-06-17"))

# read in the counterfactual production time series
counterfactual_production = readRDS("counterfactual_production.Rds")

dir.create("counterfactual_timelines")

countries_of_interest = c("USA", "GBR")

shifts = c(0, 30, 60, 90)

counterfactuals = map_dfr(shifts, function(shift_by) {
  # create the counterfactual shifted series
  cfact_with_prod = map_dfr(countries_of_interest, function(country_iso) {
    country_vacc = base_vaccination %>% filter(iso3c == country_iso)
    country_prod = counterfactual_production %>% filter(iso3c == country_iso)
    return(gen_cfact_with_prod(country_vacc, country_prod, shift_by))
  })
  cfact_with_prod = cfact_with_prod %>%
    mutate(shifted_by = as.character(shift_by))

  if (shift_by != 0) {
    saveRDS(cfact_with_prod, paste0("counterfactual_timelines/", shift_by, "_days_sooner.Rds"))
  }

  plot_cfact(cfact_with_prod, base_vaccination, counterfactual_production)

  return(cfact_with_prod)
})

# create a counterfactual with no vaccines
cfact_no_vaccines = base_vaccination %>%
  mutate(first_doses = 0, second_doses = 0, third_doses = 0)
saveRDS(cfact_no_vaccines, "counterfactual_timelines/no_vaccines.Rds")

base_vaccination = map_dfr(countries_of_interest, function(country_iso) {
  country_data = base_vaccination %>% filter(iso3c == country_iso) %>%
  mutate(total_vacc = cumsum(first_doses + second_doses + third_doses),
         date = as.Date(date)) %>%
  select(country, iso3c, date, first_doses, second_doses, third_doses, total_vacc) %>%
  mutate(shifted_by = "baseline")
})

# save the base real scenario from OWID
saveRDS(base_vaccination, "counterfactual_timelines/owid_raw.Rds")

