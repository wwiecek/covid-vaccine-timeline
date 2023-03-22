
tasks <- list('generate_counterfactuals', 'deaths_averted_plot_timeline')

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

# Generate counterfactual outputs and plots
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