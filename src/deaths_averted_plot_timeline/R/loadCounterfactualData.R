#' A function to load and summarise the deaths averted data
#' @param group_by A vector of strings of the variables to group the summary by,
#' e.g. Iso3c code, age_group or date.
#' @param quantileSamples Only used if dataset it not grouped by country. The
#' number of combinations to randomly draw. Defaults to 2000. If NULL creates
#' a wider interval based on ordering samples.
#' @param exclude_iso3cs A vector of iso3cs to exclude from the summaries,
#' default = NULL.
#' @export

# Based on loadCounterfactualData in squire.page https://github.com/mrc-ide/squire.page


loadCounterfactualDataSingle <-
  function(group_by,
           end_date = as.Date("2022-01-01"),
           quantileSamples = 2000,
           sensitivity = NULL,
           quantile = 10,
           exclude_iso3cs = NULL) {

  if(!is.null(sensitivity)){
    if(!(sensitivity %in% c('veis', 'dur_Vs','dur_Rs'))){
      stop("sensitivity parameter must be one of (NULL, 'veis', 'dur_Vs','dur_Rs')")
    }
  }

  if(!(quantile %in% c(10,90))){
    stop("quantile parameter must be one of (10,90)")
  }

  #keep country if grouping by iso3c
  group_by <- unique(c(group_by, "iso3c", "country"))

  if("week" %in% group_by){
    add_week <- TRUE
  } else{
    add_week <- FALSE
  }
  if("month" %in% group_by){
    add_month <- TRUE
  } else{
    add_month <- FALSE
  }
  #Load the Baseline data
  baseline_data <- readRDS(
   "Baseline.Rds"
  )
  if(add_week){
    #get weeks if needed
    baseline_data <- baseline_data %>%
      dplyr::mutate(
        week = as.Date(cut(date, "week"))
      )
  }
  if(add_month){
    #get weeks if needed
    baseline_data <- baseline_data %>%
      dplyr::mutate(
        month = as.Date(cut(date, "month"))
      )
  }

  grouping_base = unique(c(group_by, "replicate", "iso3c"))
  grouping_base_without_date = grouping_base[grouping_base != "date"]

  #summarise
  baseline_data <- suppressMessages(baseline_data %>%
    dplyr::filter(date <= end_date) %>%
    dplyr::group_by_at(grouping_base) %>%
    dplyr::summarise(
      baseline_infections = sum(infections),
      baseline_deaths = sum(deaths),
      baseline_total_vaccinated = sum(vaccinated_second_dose + vaccinated_booster_dose),
       # - vaccinated_second_waned - vaccinated_booster_waned),
      baseline_recovered = sum(R),
      baseline_N = mean(N)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(grouping_base_without_date) %>%
    dplyr::mutate(
      baseline_cumulative_infections = cumsum(baseline_infections),
      baseline_cumulative_deaths = cumsum(baseline_deaths),
    ) %>%
    dplyr::filter(!iso3c %in% exclude_iso3cs))

  counterfactual_data = readRDS("counterfactual_simulation.Rds")

  if(add_week){
    #get weeks if needed
    counterfactual_data <- counterfactual_data %>%
        dplyr::mutate(
          week = as.Date(cut(date, "week"))
        )
    }
  if(add_month){
      #get months if needed
      counterfactual_data <- counterfactual_data %>%
        dplyr::mutate(
          month = as.Date(cut(date, "month"))
        )
    }

  if(!is.null(sensitivity)){
    quantile_replicates <- readRDS("quantile_replicates.Rds")

    replicates <- lapply(unique(counterfactual_data$iso3c), function(iso3c){
          list(
              reps = quantile_replicates[[iso3c]][[sensitivity]][[paste0('replicates_',quantile)]],
              values = quantile_replicates[[iso3c]][[sensitivity]][[paste0('rep_',quantile,'_average')]],
              iso3c = iso3c
            )
          })

    names(replicates) = unique(counterfactual_data$iso3c)
    replicates <- bind_rows(replicates)
    replicates$iso3c <- as.character(replicates$iso3c)

    counterfactual_data <- counterfactual_data %>%
      left_join(replicates, by = "iso3c") %>%
      filter(
          (replicate == reps)
        ) %>%
      mutate(
          sensitivity_value = as.numeric(values)
        )

    baseline_data <- baseline_data %>%
      left_join(replicates, by = "iso3c") %>%
      filter(
          (replicate == reps)
        ) %>%
      mutate(
          sensitivity_value = as.numeric(values)
        )


  }

  grouping_by = unique(c(group_by, "replicate", "iso3c","counterfactual"))
  grouping_without_date = grouping_by[grouping_by != "date"]

  #summarise data
  counterfactual_data <- suppressMessages(counterfactual_data %>%
    dplyr::filter(date <= end_date) %>%
    dplyr::group_by_at(grouping_by) %>%
    dplyr::summarise(
      infections = sum(infections),
      deaths = sum(deaths),
      total_vaccinated = sum(vaccinated_second_dose + vaccinated_booster_dose), 
        # - vaccinated_second_waned - vaccinated_booster_waned),
      recovered = sum(R),
      N  = mean(N)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(grouping_without_date) %>%
    dplyr::mutate(
      cumulative_infections = cumsum(infections),
      cumulative_deaths = cumsum(deaths),
    ) %>%
    dplyr::filter(!iso3c %in% exclude_iso3cs))

  #add baseline data
  counterfactual_data <- suppressMessages(dplyr::full_join(
    counterfactual_data,
    baseline_data
  ) %>%
    dplyr::mutate(
      averted_deaths = deaths - baseline_deaths,
      averted_infections = infections - baseline_infections,
      cumulative_averted_deaths = cumulative_deaths - baseline_cumulative_deaths,
      cumulative_averted_infections = cumulative_infections - baseline_cumulative_infections
    ))
  counterfactual_data <- dplyr::group_by_at(counterfactual_data, c(group_by,"counterfactual"))

  #find median/quantile
  suppressMessages(
  counterfactual_data %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::ends_with(c("deaths", "infections", "vaccinated", "susceptible", "waned", "value")),
        .fns = list(
          avg = ~median(.x, na.rm = TRUE),
          `025` = ~quantile(.x, probs = 0.025, na.rm = TRUE),
          `975` = ~quantile(.x, probs = 0.975, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup()
  )
}
