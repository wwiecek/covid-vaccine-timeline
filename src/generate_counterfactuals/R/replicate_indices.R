## Takes an rt_optimised model fit and returns the indices for
## replicates that fall into either the bottom or top 10% 
## for their time-averaged values of the following parameters:
## 		- Vaccine efficacy against infection of two doses
##		- Duration of vaccine-derived immunity
##		- Duration of naturally-derived immunity

excess_mortality <- TRUE
if(excess_mortality){
  fit_loc <- file.path("model_fits", "excess_mortality")
  output <- file.path("counterfactual_data")
} else {
  fit_loc <- file.path("model_fits", "reported_deaths")
  output <- file.path("counterfactual_data")
}

# Choose which countries to simulate

countries_of_interest <- c('USA','GBR')

iso3cs <- gsub(".Rds", "", list.files(fit_loc))
iso3cs <- iso3cs[iso3cs %in% countries_of_interest]

if(lowtransmission){
  input_fits <- paste0(iso3cs, "_lowtransmission")
} else {
  input_fits <- iso3cs
}

# Parameters of interest

parameters <- c('dur_R', 'dur_V', 'vaccine_efficacy_infection')

# Vaccine efficacy replicates

vei_replicates <- function(out) {
	veis <- lapply(GBR$samples, function(samp){
	    mean(unlist(lapply(samp$vaccine_efficacy_infection, function(vei){
	        vei[2]
	    })))
	})
	
}