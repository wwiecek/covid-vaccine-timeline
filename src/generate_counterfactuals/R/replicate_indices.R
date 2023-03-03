## Takes an rt_optimised model fit and returns the indices for
## replicates that fall into either the bottom or top 10% 
## for their time-averaged values of the following parameters:
##      - Vaccine efficacy against infection of two doses
##      - Duration of vaccine-derived immunity
##      - Duration of naturally-derived immunity

# Given a list of replicate values, return the indices for the <10%
# and >90% quantiles

replicate_quantiles <- function(reps) {
    quants <- quantile(unlist(reps), c(0.1,0.9))
    result <- list(
            replicates_10 = which(reps <= quants[1]),
            replicates_90 = which(reps >= quants[2]),
            rep_10_average = mean(as.numeric(reps[which(reps <= quants[1])])),
            rep_90_average = mean(as.numeric(reps[which(reps >= quants[2])]))
        )
}

# Vaccine efficacies (2nd does only)

get_veis <- function(out) {
    veis <- lapply(out$samples, function(samp){
        mean(unlist(lapply(samp$vaccine_efficacy_infection, function(vei){
            vei[2]
        })))
    })
}

# Vaccine duration (2nd dose only)

get_dur_Vs <- function(out) {
    dur_vs <- lapply(out$samples, function(samp){
        mean(unlist(lapply(samp$dur_V, function(dv){
            dv[2]
        })))
    })
}

# Natural immunity duration

get_dur_Rs <- function(out) {
    dur_rs <- lapply(out$samples, function(samp){
        mean(samp$dur_R)
    })
}

dump_replicate_quantiles <- function(input_fits, output, fit_loc) {
    replicates <- lapply(input_fits, function(fit){
        fit <- readRDS(paste0(fit_loc, "/", as.character(fit), ".Rds"))

        country_reps <- list(
            veis = replicate_quantiles(get_veis(fit)),
            dur_Vs = replicate_quantiles(get_dur_Vs(fit)),
            dur_Rs = replicate_quantiles(get_dur_Rs(fit))
            )
        })

    names(replicates) <- iso3cs
    print('saving replicates')
    saveRDS(replicates, paste0(output, '/quantile_replicates.Rds'))
}