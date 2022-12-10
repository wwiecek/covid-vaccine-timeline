
## Runs predefined counterfactual simulations
## If you want to vary the counterfactuals, see the function
## `early_n` (which sets the days earlier that vaccination starts)
## and the variable `counterfactuals`

# if(!is.na(seed) & seed != "NA"){
#   set.seed(seed)
# }

cf_params <- file.path("counterfactual_timelines")

#which type of fit
excess_mortality <- TRUE
if(excess_mortality){
  fit_loc <- file.path("model_fits", "excess_mortality")
  output <- file.path("counterfactual_data")
  cf_output <- file.path("counterfactuals.Rds")
  plot_output <- file.path("fitting_plots.pdf")
  temp_plots <- file.path("temp")

} else {
  fit_loc <- file.path("model_fits", "reported_deaths")
  output <- file.path("counterfactual_data")
  cf_output <- file.path("counterfactuals.Rds")
  plot_output <- file.path("fitting_plots.pdf")
  temp_plots <- file.path("temp")
}

# Create directories

dir.create(temp_plots)
dir.create(output)


# Choose which countries to simulate

countries_of_interest <- c('USA','GBR')

iso3cs <- gsub(".Rds", "", list.files(fit_loc))
iso3cs <- iso3cs[iso3cs %in% countries_of_interest]

# Counterfactual scenarios

cfs <- gsub(".Rds", "", list.files(cf_params))

#load counterfactual simulation functions
#' Generate Deaths Averted
#'
#' @param out Output of `squire::pmcmc`
#' @param draws Number of draws from mcmc chain. Default = 10
#' @param counterfactual A named list of lists. Each list should contain the
#' dates that vaccines change, the number of vaccines, and efficacies. If NULL
#' that counter-factual will be skipped.
#' @param reduce_age Should the output be aggregated by age? Can be TRUE, FALSE,
#' or "Both" to return values both with the aggregated in a special age_group "Total"
#' @param direct Should there be an estimate of direct effect of the vaccine, i.e.
#' no protection against infection. Default = FALSE.
#' @param plot_name Name for fitting plot of country, if NULL no plot is made.
deaths_averted <- function(out, draws, counterfactual, iso3c, reduce_age = TRUE,
                           direct = FALSE, plot_name = NULL, excess = TRUE) {
  #error if Baseline in counterfactual
  if(any(c("Baseline","baseline","BASELINE") %in% names(counterfactual))){
    stop('"Baseline" is a reserved name for a counterfactual, please choose another name')
  }

  # return NULL if nothing
  if(!("pmcmc_results" %in% names(out))) {
    return(NULL)
  }

  # get the real data
  data <- out$pmcmc_results$inputs$data
  country <- out$parameters$country
  # iso3c <- squire::population$iso3c[squire::population$country == country][1]
  if(is.null(suppressWarnings(data$week_end))){
    date_0 <- max(data$date)
  } else {
    date_0 <- max(data$week_end)
  }
  #draw the parameters
  if(is.null(draws)){
    pars.list <- NULL
  } else {
    pars.list <- squire.page::generate_parameters(out, draws)
  }

  #Set up the baseline results
  baseline <- squire.page::generate_draws(out, pars.list, draws)

  #create the fitting plot if needed
  if(!is.null(plot_name)){
    fit_1 <- dp_plot_2(baseline, excess) + ggplot2::labs(
      title = country
    )
    fit_2 <- squire.page::cdp_plot(baseline) +
      ggplot2::ylab("Cumulative Daily Deaths")
    if(excess){
      text <- ggpubr::text_grob(
        "Daily deaths are included with estimated and reported weekly excess mortality
 shown as dashed black lines. The red line represents median model estimates of
 daily deaths, and the shaded region represents the 95% quantiles of the
 estimated deaths. Plots of the cumulative estimated deaths are also shown."
      )
    } else {
      text <- ggpubr::text_grob(
        "Daily deaths are included with reported COVID-19 deaths shown as points. The
 red line represents median model estimates of daily deaths, and the shaded
 region represents the 95% quantiles of the model estimated deaths. Plots of the
 cumulative estimated deaths are also shown, with the cumulative number of
 reported deaths represented by a dashed black line."
      )
    }
    plot <- ggpubr::ggarrange(
      fit_1,
      fit_2,
      text,
      ncol = 1,
      heights = c(1,1,0.4)
    )
    #save to requested location
    ggplot2::ggsave(plot_name, plot)
  }

  # format the counter factual run
  baseline_deaths <- squire.page::nimue_format(baseline, c("deaths", "infections"), date_0 = date_0,
                                               reduce_age = reduce_age) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
    na.omit() %>%
    dplyr::mutate(counterfactual = "Baseline")

  if(!reduce_age){
    baseline_deaths <- dplyr::mutate(baseline_deaths, age_group = as.character(.data$age_group))
  }

  baseline_deaths$t <- NULL

  dataframeLength <- nrow(baseline_deaths)

  #set up data frame to hold results
  columns <- ncol(baseline_deaths)
  deaths_df <- as.data.frame(
    matrix(NA,
           ncol = columns,
           nrow = dataframeLength*length(counterfactual))
  )

  names(deaths_df) <- names(baseline_deaths)
  class(deaths_df$date) <- "Date"

  #run the counter-factual
  for(counterIndex in seq_along(counterfactual)){
    #generate draws with pars.list
    if(!is.null(counterfactual[[counterIndex]])){
      counter <- squire.page::generate_draws(out = update_counterfactual(out, counterfactual[[counterIndex]]),
                                             pars.list = pars.list, draws = draws)
      #format the counter factual run
      counter_df <- squire.page::nimue_format(counter, c("deaths", "infections"),
                                              date_0 = date_0,
                                              reduce_age = reduce_age) %>%
        dplyr::distinct() %>%
        tidyr::pivot_wider(names_from = .data$compartment, values_from = .data$y) %>%
        na.omit() %>%
        dplyr::mutate(counterfactual = names(counterfactual)[counterIndex])

      if(!reduce_age){
        counter_df <- dplyr::mutate(counter_df, age_group = as.character(.data$age_group))
      }
      counter_df$t <- NULL

      #attach to counter-factual data frame
      deaths_df[seq(dataframeLength) + dataframeLength*(counterIndex-1),] <- counter_df
    }
  }

  #remove unused row (i.e. if a counter factual is null)
  deaths_df <- deaths_df %>%
    dplyr::filter(!is.na(.data$counterfactual))

  #add baseline data
  deaths_df <- rbind(
    deaths_df,
    baseline_deaths
  )

  if(!is.null(out$interventions$pre_epidemic_isolated_deaths)){
    if(out$interventions$pre_epidemic_isolated_deaths > 0){
      if(reduce_age){
        deaths_df <- rbind(deaths_df,
                           expand.grid(replicate = unique(deaths_df$replicate),
                                       counterfactual = unique(deaths_df$counterfactual)) %>%
                             dplyr::mutate(
                               date = NA,
                               deaths = out$interventions$pre_epidemic_isolated_deaths,
                               infections = 0
                             )
        )
      } else {
        deaths_df <- rbind(deaths_df,
                           expand.grid(replicate = unique(deaths_df$replicate),
                                       counterfactual = unique(deaths_df$counterfactual),
                                       age_group = unique(deaths_df$age_group)) %>%
                             dplyr::group_by(replicate, counterfactual) %>%
                             dplyr::mutate(
                               date = NA,
                               deaths = out$interventions$pre_epidemic_isolated_deaths/length(age_group),
                               infections = 0
                             ) %>% dplyr::ungroup()
        )
      }
    }
  }

  deaths_df <- dplyr::arrange(deaths_df, counterfactual, replicate, date)

  # and add country info
  deaths_df$country <- country
  deaths_df$iso3c <- iso3c
  return(deaths_df)
}

remove_indirect <- function(out){
  #we update the efficacies in the interventions
  #just set ve_i's to 0, no need to scale disease as this is done later in ll func
  for(var in grep("ve_i", names(out$interventions$vaccine_efficacies))){
    out$interventions$vaccine_efficacies[[var]] <- rep(0, 3)
    out$pmcmc_results$inputs$interventions$vaccine_efficacies[[var]] <- rep(0, 3)
  }

  #set relative infectiousness to 1
  out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated <-
    matrix(1,
           nrow = nrow(out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated),
           ncol = ncol(out$pmcmc_results$inputs$model_params$rel_infectiousness_vaccinated)
    )
  out$odin_parameters$rel_infectiousness_vaccinated <-
    matrix(1,
           nrow = nrow(out$odin_parameters$rel_infectiousness_vaccinated),
           ncol = ncol(out$odin_parameters$rel_infectiousness_vaccinated)
    )
  out$parameters$rel_infectiousness_vaccinated <-
    rep(1, length(out$parameters$rel_infectiousness_vaccinated))
  return(out)
}
remove_healthcare <- function(out){
  out$parameters$hosp_bed_capacity <- 10^7
  out$parameters$ICU_bed_capacity <- 10^7
  out$odin_parameters$hosp_beds <- 10^7
  out$odin_parameters$ICU_beds <- 10^7
  out$interventions$hosp_bed_capacity <- 10^7
  out$interventions$ICU_bed_capacity <- 10^7
  out$pmcmc_results$inputs$model_params$hosp_beds <- 10^7
  out$pmcmc_results$inputs$model_params$ICU_beds <- 10^7
  out$pmcmc_results$inputs$interventions$hosp_bed_capacity <- 10^7
  out$pmcmc_results$inputs$interventions$ICU_bed_capacity <- 10^7
  return(out)
}
update_counterfactual <- function(out, counterfactual){

  # For some reason, max_vaccine needs to be longer than everything else

  # if (length(counterfactual$max_vaccine) == length(counterfactual$date_vaccine_change)) {
  #   counterfactual$max_vaccine <- append(0,counterfactual$max_vaccine)
  # }

  out$pmcmc_results$inputs$interventions$date_vaccine_change <-
    counterfactual$date_vaccine_change
  out$pmcmc_results$inputs$interventions$date_vaccine_efficacy <-
    counterfactual$date_vaccine_efficacy
  out$pmcmc_results$inputs$interventions$max_vaccine <-
    counterfactual$max_vaccine
  out$pmcmc_results$inputs$interventions$dose_ratio <-
    counterfactual$dose_ratio

  #don't need to update the max vaccine as it uses intervention data


  out$interventions$date_vaccine_change <-
    counterfactual$date_vaccine_change
  out$interventions$date_vaccine_efficacy <-
    counterfactual$date_vaccine_efficacy
  out$interventions$max_vaccine <-
    counterfactual$max_vaccine
  out$interventions$dose_ratio <-
    counterfactual$dose_ratio

  # Update vaccine duration
  if (!is.null(counterfactual$dur_V)){
    out$parameters$dur_V <- counterfactual$dur_V
    out$odin_parameters$gamma_vaccine[4:5] <- 2*1/counterfactual$dur_V
    out$pmcmc_results$inputs$model_params$gamma_vaccine[4:5] <- 2*1/counterfactual$dur_V
  }



  #also remove healthcare if requested
  if(!is.null(counterfactual$no_healthcare)){
    if(counterfactual$no_healthcare){
      out <- remove_healthcare(out)
    }
  }
  return(out)
}

#updated version of dp plot to make it similar to cdp_plot
dp_plot_2 <- function (res, excess) {
  date_0 <- squire.page:::get_data_end_date.excess_nimue_simulation(res)
  data <- res$pmcmc_results$inputs$data
  #data$date <- squire.page:::get_dates_greater.excess_nimue_simulation(res)
  data$adjusted_deaths <- data$deaths/as.numeric(data$week_end -
                                                   data$week_start)
  suppressWarnings(dp <- plot(res, "deaths", date_0 = date_0,
                              x_var = "date") + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
                                                                                     axis.title.x = ggplot2::element_blank()) + ggplot2::ylab("Daily Deaths") +
                     ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
                     ggplot2::xlab(""))
  if(excess){
    dp + ggplot2::geom_segment(data = data,
                               ggplot2::aes(x = .data$week_start, xend = .data$week_end,
                                            y = .data$adjusted_deaths, yend = .data$adjusted_deaths),
                               linetype = "dashed")
  } else {
    dp + ggplot2::geom_point(data = data,
                             ggplot2::aes(x = .data$week_end, y = .data$adjusted_deaths),
                             size = 1)
  }
}


# Load counterfactuals
# TODO: this needs to be changed when we start handling third doses

load_counterfactuals <- function(cf, iso3c_in) {
  fit <- readRDS(paste0(fit_loc, "/", iso3c_in, ".Rds"))
  max_date <- max(fit$interventions$date_vaccine_change)
  cf_data <- readRDS(paste0(cf_params,"/",cf,".Rds")) %>%
    filter(iso3c == iso3c_in) %>%
    mutate(second_dose_ratio = replace(cumsum(second_doses)/max(cumsum(first_doses),1),1,0))
  cfs <- cf_data %>%
    filter(date <= as.character(max_date)) %>%
    rename(
        max_vaccine = first_doses,
        dose_ratio = second_dose_ratio,
        date_vaccine_efficacy = date
      ) %>%
    mutate (
      date_vaccine_efficacy = as.Date(date_vaccine_efficacy,'%Y-%m-%d')
      ) %>%
    mutate (
      date_vaccine_change = date_vaccine_efficacy
      ) %>%
    select(max_vaccine,dose_ratio,date_vaccine_efficacy,date_vaccine_change)
  cfs = as.list(cfs)
  # For some unknown reason, this needs to be longer
  cfs$max_vaccine <- append(0,cfs$max_vaccine)
  return(cfs)
}

counterfactuals <- lapply(iso3cs,function(iso3c) {

  # No vaccine counterfactual

  c0 <- list(max_vaccine = c(0,0),
             date_vaccine_change = as.Date(Sys.Date()) - 1,
             dose_ratio = 0,
             date_vaccine_efficacy = as.Date(Sys.Date()) - 1,
             dur_V = 446)
  cf_scenarios <- lapply(cfs, function(cf){
    return(load_counterfactuals(cf,iso3c))
    })
  names(cf_scenarios) <- cfs
  cf_list <- append(list(`No Vaccines`=c0),cf_scenarios)

  return(cf_scenarios)
  })
names(counterfactuals) <- iso3cs

#simplify submission (hold over from using cluster)
submission_lists <- map(
  iso3cs,
  ~list(
    iso3c = .x,
    counterfactual = counterfactuals[[.x]],
    excess = excess_mortality
  )
)

#this will take a long time, originally run on a cluster, too memory intensive to be run in parrallel
df_out <- map_dfr(submission_lists, function(sub_list){

  out <- readRDS(paste0(fit_loc, "/", as.character(sub_list$iso3c), ".Rds"))

  df <- deaths_averted(out, draws = NULL,
                   counterfactual = sub_list$counterfactual,
                   iso3c = sub_list$iso3c,
                   reduce_age = TRUE,
                   direct = sub_list$excess,
                   plot_name = paste0(temp_plots, "/", sub_list$iso3c, ".pdf"),
                   excess = sub_list$excess)
})

df_baseline <- df_out %>%
  filter(counterfactual == 'Baseline')

df_cf <- df_out %>%
  filter(counterfactual != 'Baseline')

saveRDS(df_baseline,paste0(output,'/Baseline.Rds'))
saveRDS(df_cf,paste0(output,'/counterfactual_simulation.Rds'))

#combine outputs into final objects
qpdf::pdf_combine(list.files(temp_plots, full.names = TRUE), plot_output)

#save raw excess mortality
if(excess){
  map_dfr(iso3cs, function(iso3c){
    #get fit
    readRDS(paste0(
      fit_loc, "/", iso3c, ".Rds"
    ))$pmcmc_results$inputs$data
  }, .id = "iso3c") %>%
    group_by(week_start, week_end) %>%
    summarise(
      deaths = sum(deaths)
    ) %>%
    mutate(
      obsDate = (week_end - week_start)/2 + week_start,
      deaths = deaths/as.numeric(week_end - week_start)
    ) %>%
    ungroup() %>%
    select(obsDate, deaths) %>%
    saveRDS("excess_deaths.Rds")

}
