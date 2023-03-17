## Runs predefined counterfactual simulations
## If you want to vary the counterfactuals, see the function
## `early_n` (which sets the days earlier that vaccination starts)
## and the variable `counterfactuals`

# # Use to switch into this file's directory for development
# root = here::here()
# setwd(file.path(root, "src/generate_counterfactuals"))
# setwd(root) # Use to get back to root


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

if(lowtransmission){
  input_fits <- paste0(iso3cs, "_lowtransmission")
} else {
  input_fits <- iso3cs
}

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

  # get the real data
  data <- out$inputs$data
  country <- out$parameters$country

  if(is.null(suppressWarnings(data$date_end))){
    date_0 <- max(data$date)
  } else {
    date_0 <- max(data$date_end)
  }

  date_start <- min(data$date_start)

  t_end <- as.integer(max(counterfactual$owid_raw$date) - out$inputs$start_date)

  # Add the rt_optimised class label to try to prevent a failure of the generic function
  # generate_draws
  attr(out, "class") <- c("rt_optimised",class(out))

  #Set up the baseline results
  baseline <- squire.page::generate_draws(out, t_end)
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

    # We need to add this class so that the nimue_format utility knows it's a booster sim
  attr(baseline, "class") <- c("lmic_booster_nimue_simulation",class(baseline))


  # format the counter factual run
  baseline_deaths <- squire.page::nimue_format(baseline, c("deaths", "infections", "vaccinated_first_dose",
        "vaccinated_second_dose", "vaccinated_second_waned", "N", "R"), date_0 = date_start,
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

      cf_out <- update_counterfactual(out, counterfactual[[counterIndex]])

      counter <- squire.page::generate_draws(out = cf_out, t_end)

      # We need to add this class so that the nimue_format utility knows it's a booster sim
      attr(counter, "class") <- c("lmic_booster_nimue_simulation",class(counter))

      #format the counter factual run
      counter_df <- squire.page::nimue_format(counter, c("deaths", "infections", "vaccinated_first_dose",
       "vaccinated_second_dose", "vaccinated_second_waned", "N", "R"),
                                              date_0 = date_start,
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

  deaths_df <- dplyr::arrange(deaths_df, counterfactual,
    N, R, replicate, vaccinated_second_dose, vaccinated_second_waned, date)

  # and add country info
  deaths_df$country <- country
  deaths_df$iso3c <- iso3c
  return(deaths_df)
}

update_counterfactual <- function(out, counterfactual){

  tt_primary_doses <- as.integer(counterfactual$date - out$inputs$start_date)

  # replace the vaccination data with the counterfactual data

  out$parameters$primary_doses <- c(0,counterfactual$first_doses)
  out$parameters$tt_primary_doses <- c(0,tt_primary_doses)
  if(boosters) {
    out$parameters$booster_doses <- c(0,counterfactual$third_doses)
    out$parameters$tt_booster_doses <- c(0,tt_primary_doses)
  } else {
    out$parameters$booster_doses <- 0
    out$parameters$tt_booster_doses <- 0
  }

  return(out)
}

#updated version of dp plot to make it similar to cdp_plot
dp_plot_2 <- function (res, excess) {
  data <- res$inputs$data
  data$adjusted_deaths <- data$deaths/as.numeric(data$date_end -
                                                   data$date_start)
  suppressWarnings(dp <- plot(res, particle_fit=TRUE) + ggplot2::theme_bw() + ggplot2::theme(legend.position = "none",
                                                                                     axis.title.x = ggplot2::element_blank()) + ggplot2::ylab("Daily Deaths") +
                     ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
                     ggplot2::xlab(""))
  if(excess){
    dp + ggplot2::geom_segment(data = data,
                               ggplot2::aes(x = .data$date_start, xend = .data$date_end,
                                            y = .data$adjusted_deaths, yend = .data$adjusted_deaths),
                               linetype = "dashed")
  } else {
    dp + ggplot2::geom_point(data = data,
                             ggplot2::aes(x = .data$date_end, y = .data$adjusted_deaths),
                             size = 1)
  }
}

# Get max date

max_date <- function(out) {
  as.Date(max(out$inputs$start_date) + max(out$parameters$tt_primary_doses),"%Y-%m-%d")
}

# Load counterfactuals

load_counterfactuals <- function(cf, iso3c_in) {
  fit <- readRDS(paste0(fit_loc, "/", iso3c_in, ".Rds"))
  end_date <- max_date(fit)
  cf_data <- readRDS(paste0(cf_params,"/",cf,".Rds")) %>%
    filter(iso3c == iso3c_in)
  cfs <- cf_data  %>%
    mutate (
      date = as.Date(date,'%Y-%m-%d')
      ) %>%
    filter(date <= as.Date(end_date)) %>%
    select(first_doses, third_doses, date)
  cfs = as.list(cfs)
  return(cfs)
}

counterfactuals <- lapply(iso3cs, function(iso3c) {

  # No vaccine counterfactual

  c0 <- list(first_doses = c(0),
             date = as.Date(Sys.Date()) - 1,
             third_doses = c(0))
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
  input_fits,
  ~list(
    iso3c = .x,
    counterfactual = counterfactuals[[gsub("_lowtransmission", "", .x)]],
    excess = excess_mortality
  )
)

#this will take a long time, originally run on a cluster, too memory intensive to be run in parrallel
df_out <- map_dfr(submission_lists, function(sub_list){

  out <- readRDS(paste0(fit_loc, "/", as.character(sub_list$iso3c), ".Rds"))

  iso3c <- gsub("_lowtransmission", "", sub_list$iso3c)

  df <- deaths_averted(out, draws = NULL,
                   counterfactual = sub_list$counterfactual,
                   iso3c = iso3c,
                   reduce_age = TRUE,
                   direct = sub_list$excess,
                   plot_name = paste0(temp_plots, "/", iso3c, ".pdf"),
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

dump_replicate_quantiles(input_fits, output, fit_loc)
