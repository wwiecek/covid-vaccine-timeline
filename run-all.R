# Use this master script to generate all of the results
# see README for details

# Packages ------
devtools::install_github("mrc-ide/nimue")
devtools::install_github("mrc-ide/squire")
devtools::install_github(
  "mrc-ide/squire.page",
  ref = "fc27dcabec55d70ec293328813cec644c56a63db"
)
devtools::install_github(
  "mrc-ide/drjacoby",
  ref = "476d94f3eb7357f8e2278834c0af04afd772cf69"
)


# Generate vaccination timelines and input data -----
ord_runs <- lapply(
  list(
    "parameters_vaccines",
    "input_excess_mortality",
    "input_jhu",
    "input_sequencing",
    "input_vaccinations",
    "generate_production_counterfactual",
    "generate_vaccine_counterfactual"
  ),
  function(x) {
    cm <- orderly::orderly_run(x)
    orderly::orderly_commit(cm)
  }
)

# Generate Rt fits -----

date <- '2023-01-01'

iso3cs <- c("USA", "GBR")

rt_params_lists <- lapply(iso3cs, function(iso3c){
  list(
      iso3c = iso3c
      date = date,
      samples = 32*3, #how many random samples to generate and fit
      seed = FALSE, #Set a seed, useful for debugging
      parallel = TRUE,
      #Should we build the required documentation+data for the fit
      document = TRUE,
      fit_cases = FALSE #issues with this, don't have time to fix them.
      # #Fitting parameters (leave as blank to use defaults in fitting_params.Rds)
      # initial_infections_interval = c(5, 500),
      # n_particles = 10,
      # k = 14,
      # rt_interval = c(0.5, 10)
    )
})

for (rt_params in rt_params_lists) {
  rt_id <- orderly::orderly_run(
    "lmic_reports_rt_optimise",
    parameters = rt_params)

  orderly::orderly_commit(rt_id)
}

# Generate and plot infection timelines -----

tasks <- list(
  "generate_counterfactuals",
  "deaths_averted_plot_timeline")

parameter_sets <- list(
    list(
      excess=FALSE, 
      boosters=TRUE,
      double_boosters = FALSE
    ),
    list(
      excess=TRUE, 
      boosters=TRUE,
      double_boosters = FALSE
    ),
    list(
      excess=TRUE, 
      boosters=TRUE,
      double_boosters = TRUE
    ) 
  )

ord_runs <- lapply(
  tasks,
  function(task) {
    lapply(
      parameter_sets,
      function(params) {
        cm <- orderly::orderly_run(task, parameters = params)
        orderly::orderly_commit(cm)
      })
  }
)

preprint <- orderly::orderly_run("preprint")
orderly::orderly_commit(preprint)