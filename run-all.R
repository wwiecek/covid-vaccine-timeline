# Use this master script to generate all of the results
# see README for details

# Packages ------
devtools::install_github("mrc-ide/nimue")
devtools::install_github("mrc-ide/squire")
devtools::install_github(
  "mrc-ide/squire.page",
  ref = "3fbb97f3e72c4e6bd7f66690a53f7bb59ce5ea0e"
)
devtools::install_github(
  "mrc-ide/drjacoby",
  ref = "476d94f3eb7357f8e2278834c0af04afd772cf69"
)


# Generate all outputs -----
ord_runs <- lapply(
  list(
    "generate_production_counterfactual",
    "generate_vaccine_counterfactual",
    "generate_counterfactuals",
    "deaths_averted_plot_timeline"
  ),
  function(x) {
    cm <- orderly::orderly_run(x)
    orderly::orderly_commit(cm)
  }
)

