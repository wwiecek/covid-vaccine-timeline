## Adding counterfactual scenarios

To add a counterfactual scenario, you have to add a vaccination timeseries to the folder `../data/raw/counterfactual_timelines` (see [format](#counterfactual-vaccine-timeline-format) below). If you have not added a timeseries previously, you will need to create the `counterfactual_timelines`. Following that, the following files need to be updated:

 - `gather_simulations_time/orderly.yml`: add the required output filenames to the `artefacts` section
 - `deaths_averted_plot_timeline/orderly.yml`: add the required output filenames to the `artefacts` section
 and the required input files to the `depends` section
 - `deaths_averted_plot_timeline/script.R`: add the required counterfactual names (input files with `.Rmd` stripped) to the cfs vector

### Counterfacutal vaccine timeline format

Each counterfactual scenario has its own file so that adding counterfactual scenarios is just a matter of adding new data files. Files should be csv file with the following properties:
 -  **Name:** filename should identify counterfactual assumptions, e.g. if a counterfactual is 30 days earlier start of vaccination and no change to the administration over time after that the file could be `30-days-early-baseline-admin.Rds` (the exact scheme depends on what counterfactual scenarios are ultimately going to be considered)
 - Columns:
    - `date`: date in yyyy/mm/dd format. For each country and each counterfactual scenario, the first date entry should be at or before the first day on which the vaccination campaign started and the last required date entry is 2022/01/01
    - `country`: country name, as it appears in the [ISO 3166-1 alpha-3 list](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Current_codes) on Wikipedia
    - `iso3c`: [ISO 3166-1 alpha-3 letter country code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
    - `first_doses`: Number of first doses administered
    - `second_doses`: Number of second doses administered
    - `third_doses`: Number of third (booster) doses administered

See `owid-raw.csv` for a file of the required format with the real vaccination campaign data derived from the set provided by [Our World in Data](https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations).


To see a vignette illustrating how to generate and plot counterfactuals, run `rmarkdown::render('./src/sim_plot_counterfactuals.Rmd')`. 