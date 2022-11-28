# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/generate_production_counterfactual"))
# setwd(root) # Use to get back to root


#' Takes a numerical vector and stretches it by linearly interpolating from
#' surrounding data.
#' @param vec Vector to stretch
#' @param shift_n By how many elements to stretch
interpolate = function(vec, shift_n) {
  old_vector_length = length(vec)
  new_vector_length = old_vector_length + shift_n
  # fitting the new number of points on the x axis in regular intervals
  # we get a sequence of points for which we can then calculate y values
  x_positions = seq(1, old_vector_length, length.out = new_vector_length)

  new_vector = numeric(new_vector_length)

  for (index in 1:new_vector_length) {
    x_position = x_positions[index]

    # first and last value is the same as in old vector
    if (index == 1 | index == new_vector_length) {
      new_vector[index] = vec[x_position]
    } else {
      bottom_index = floor(x_position)
      top_index = bottom_index + 1
      from_top = x_position %% 1
      from_bottom = 1 - from_top

      # interpolate from the two corresponding surrounding values
      interpolated_value = vec[bottom_index] * from_bottom + vec[top_index] * from_top

      new_vector[index] = interpolated_value
    }
  }

  return(new_vector)
}

#' Prepends manufacturing time series by interpolated production values
#' @param production_data True baseline production data for a country
#' @param shift_n How many days into the past should it shift the production
#' @param interpolation_end_date Last day from baseline data to use for interpolation
#' @return Elongated production data with early data stretched / interpolated
generate_counterfactual_series = function(production_data,
                                          shift_n,
                                          interpolation_end_date) {
  before_source_end = production_data %>% filter (date <= interpolation_end_date)
  after_source_end = production_data %>% filter (date > interpolation_end_date)

  production_before_source_end = before_source_end$cumulative_available_vaccines
  production_after_source_end = after_source_end$cumulative_available_vaccines

  interpolated_production = interpolate(production_before_source_end, shift_n)

  baseline_start_date = production_data$date[1]
  pre_pad_df = data.frame(
    date = seq(baseline_start_date - shift_n, baseline_start_date - 1, by = "days"),
    cumulative_available_vaccines = 0,
    country = production_data$country[1],
    iso3c = production_data$iso3c[1]
  )

  elongated_production_data = bind_rows(pre_pad_df, production_data)

  elongated_production_data$cumulative_available_vaccines =
    c(interpolated_production, production_after_source_end)

  return(elongated_production_data)
}

### RUN
# the parameter enters as string, change to Date
interpolation_source_end = as.Date(interpolation_source_end)

baseline_prod = readRDS("baseline_vaccine_production.Rds")

countries = unique(baseline_prod$country)

baseline_data_by_country = map(countries, function (country_name) {
  country_production_data = baseline_prod %>% filter(country == country_name)
  return (country_production_data)
})

counterfactual_production = map_dfr(
  baseline_data_by_country,
  generate_counterfactual_series,
  shift_by,
  interpolation_source_end
)

saveRDS(counterfactual_production, "counterfactual_production.Rds")
