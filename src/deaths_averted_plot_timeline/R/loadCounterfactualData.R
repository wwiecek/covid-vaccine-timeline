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


loadCounterfactualDataSingle <- function(group_by, quantileSamples = 2000,
                                   exclude_iso3cs = NULL){
  #keep country if grouping by iso3c
  if("iso3c" %in% group_by | "country" %in% group_by){
    group_by <- unique(c(group_by, "iso3c", "country"))
    normalMedian <- TRUE
  } else{
    normalMedian <- FALSE
  }
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
  #summarise
  baseline_data <- suppressMessages(baseline_data %>%
    dplyr::group_by_at(unique(c(group_by, "replicate", "iso3c"))) %>%
    dplyr::summarise(
      baseline_infections = sum(.data$infections),
      baseline_deaths = sum(.data$deaths),
      baseline_vaccinated = sum(.data$vaccinated_second_dose),
      baseline_vaccinated_second_waned = sum(.data$vaccinated_second_waned),
      baseline_recovered = sum(.data$R),
      baseline_N = mean(.data$N),
      baseline_percent_susceptible = ((baseline_N - baseline_recovered) * 
        (1 - 0.75*(baseline_vaccinated/baseline_N - baseline_vaccinated_second_waned/baseline_N)
           - 0.15*baseline_vaccinated_second_waned/baseline_N)/baseline_N)
      )  %>%
    dplyr::filter(!.data$iso3c %in% exclude_iso3cs))

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

  #summarise data
  counterfactual_data <- suppressMessages(counterfactual_data %>%
    dplyr::group_by_at(unique(c(group_by, "replicate", "iso3c","counterfactual"))) %>%
    dplyr::summarise(
      infections = sum(.data$infections),
      deaths = sum(.data$deaths),
      vaccinated = sum(.data$vaccinated_second_dose),
      vaccinated_second_waned = sum(.data$vaccinated_second_waned),
      recovered = sum(.data$R),
      N  = mean(.data$N),
      percent_susceptible = ((N - recovered) * (1 - 0.75*(vaccinated/N
       - vaccinated_second_waned/N) - 0.15*(vaccinated_second_waned/N))/N)
    ) %>%
    dplyr::filter(!.data$iso3c %in% exclude_iso3cs))

  #add baseline data
  counterfactual_data <- suppressMessages(dplyr::full_join(
    counterfactual_data,
    baseline_data
  ) %>%
    dplyr::mutate(
      averted_deaths = .data$deaths - .data$baseline_deaths,
      averted_infections = .data$infections - .data$baseline_infections
    ))
  counterfactual_data <- dplyr::group_by_at(counterfactual_data, c(group_by,"counterfactual"))
  #to find median/quantiles
  if(!normalMedian){
    if(!is.null(quantileSamples)){
      #else we randomly combine replicates and find their median/quantile over that
      noReplicates <- length(unique(counterfactual_data$replicate))
      uniqueCountries <- sort(unique(counterfactual_data$iso3c))
      noCountries <- length(uniqueCountries)
      #the median/quantiles of that
      counterfactual_data <- counterfactual_data %>% #remove previous groupings:
        dplyr::mutate(countryNumber =
                        sapply(.data$iso3c,
                               function(x){
                                 which(uniqueCountries == x)
                               },
                               USE.NAMES = F)
        )
      #generate the random draws
      #data set to hold results
      #use group meta data to get output length
      meta_data <- dplyr::group_data(counterfactual_data)
      sample_length <- nrow(meta_data)
      sample_names <- c(group_by, "counterfactual",
                        "deaths", "infections",
                        "vaccinated",
                        "vaccinated_second_waned",
                        "percent_susceptible",
                        "baseline_deaths", "baseline_infections",
                        "baseline_vaccinated",
                        "baseline_vaccinated_second_waned",
                        "baseline_percent_susceptible",
                        "averted_deaths", "averted_infections"
      )
      samples <- counterfactual_data[1,] %>%
        dplyr::select(dplyr::all_of(sample_names))

      samples[1:(quantileSamples*sample_length),] <- NA

      for(i in 1:quantileSamples){
        #get random draw
        replicates <- sample.int(noReplicates, size = noCountries, replace = T)

        sum_data <- suppressMessages(counterfactual_data %>% #reduce to replicates used:
                                       dplyr::filter(
                                         .data$replicate == replicates[.data$countryNumber]
                                       ) %>% #sum over the given groupings
                                       dplyr::summarise(
                                         dplyr::across(
                                           c(.data$deaths, .data$infections, 
                                             .data$vaccinated, .data$percent_susceptible,
                                             .data$vaccinated_second_waned,
                                             .data$baseline_deaths, .data$baseline_infections,
                                             .data$baseline_vaccinated, .data$baseline_percent_susceptible,
                                             .data$baseline_vaccinated_second_waned,
                                             .data$averted_deaths, .data$averted_infections),
                                           ~sum(.x, na.rm = TRUE)
                                         )
                                       ))
        #add to samples
        samples[seq(min(sample_length,nrow(sum_data))) + sample_length*(i-1),] <- sum_data
      }
    } else {
      #otherwise we calculate a wider quantile
      #reorder replicates
      samples <- counterfactual_data %>% dplyr::arrange(.data$iso3c, .data$averted_deaths) %>%
        dplyr::mutate(replicate = (seq_along(.data$iso3c) - 1) %% length(unique(.data$replicate)))

      #group by groupings and other replicates
      samples <- dplyr::group_by_at(samples,
                                    c(group_by, "counterfactual", "replicate")) %>%
        #sum across replicates
        dplyr::summarise(
          dplyr::across(
            c(.data$deaths, .data$infections, 
              .data$vaccinated, .data$percent_susceptible,
              .data$vaccinated_second_waned,
              .data$baseline_deaths, .data$baseline_infections,
              .data$baseline_vaccinated, .data$baseline_percent_susceptible,
              .data$baseline_vaccinated_second_waned,
              .data$averted_deaths, .data$averted_infections),
            ~sum(.x, na.rm = TRUE)
          ),
          .groups = "drop_last"
        )
    }
    #re-add grouping
    counterfactual_data <- samples %>% #remove missing data
      stats::na.omit()
    remove(samples)
  }
  #find median/quantile
  suppressMessages(
  counterfactual_data %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::ends_with(c("deaths", "infections", "vaccinated", "susceptible", "waned")),
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