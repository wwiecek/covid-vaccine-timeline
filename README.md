# covid-vaccine-timelines

This is an [orderly](https://github.com/vimc/orderly) project. The
directories are:

`src`: tasks used to generate the reports

`vignettes`: guidance on running the tasks

`data`: Contains the following data:

-   `excess_mortality`: Fitted *nimue* models and pre-generated
    simulations, along with vaccine allocation information, for the fits
    to excess mortality.
-   `reported_deaths`: Fitted *nimue* models and pre-generated
    simulations, along with vaccine allocation information, for the fits
    to reported COVID deaths.
-   `raw`: Raw data used in modelling:
    -   `owid.rds`: Our World In Data dataset used for vaccine
        allocation, downloaded 13-02-2022
    -   `excess_deaths.rds`: Excess death estimates from the Economist,
        downloaded 13-02-2022
    -   `combined_data.Rds`: Reported COVID deaths dataset, downloaded
        13-02-2022
    -   `vaccine_agreements.rds, vaccine_doses_by_manufacturer.rds, who_vacc.rds, who_vacc_meta.rds`:
        Other vaccination datasets, downloaded 13-02-2022
    -   `worldsf.Rds`: World map sf used in the plotting, downloaded
        20-04-2022 from
        <https://datahub.io/core/geo-countries/r/countries.geojson>
    -   `generate_counterfactuals.R`: R code used to generate the
        simulations from the model fits
    -   `counterfactual_timelines`: folder for the set of counterfactual timelines proposed by scenario analysis

`exploration`: Scripts to organise data and visualise the output of counterfactuals that aren't
integrated into orderly tasks

The purpose of this repository is to estimate the number of deaths that
could have been averted by COVID-19 vaccinations if they had begun at earlier dates. This utilises the
[nimue](https://github.com/mrc-ide/nimue) fits generated in
[global-lmic-reports-orderly](https://github.com/mrc-ide/global-lmic-reports-orderly),
which are also used to produce the reports
[here](https://mrc-ide.github.io/global-lmic-reports/). It is based on the simulations of
deaths averted by COVID-19 vaccines found [here](https://github.com/mrc-ide/covid-vaccine-impact-orderly)

## Installation

    git clone https://github.com/davidoj/covid-vaccine-timeline
    cd covid-vaccine-timeline
    open covid-vaccine-impact-orderly.Rproj

## Usage

A vignette that briefly runs through generating counterfactual simulations and producing some rough plots can be found in the `vignettes` folder.

## Counterfacutal vaccine timeline format

Each counterfactual scenario has its own file so that adding counterfactual scenarios is just a matter of adding new data files. Files should be csv file with the following properties:
 -  **Name:** filename should identify counterfactual assumptions, e.g. if a counterfactual is 30 days earlier start of vaccination and no change to the administration over time after that the file could be `30days_badmin.Rds` (the exact scheme depends on what counterfactual scenarios are ultimately going to be considered)
 - Columns:
    - `date`: date in yyyy/mm/dd format. For each country and each counterfactual scenario, the first date entry should be at or before the first day on which the vaccination campaign started and the last required date entry is 2022/01/01
    - `country`: country name, as it appears in the [ISO 3166-1 alpha-3 list](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Current_codes) on Wikipedia
    - `iso3c`: [ISO 3166-1 alpha-3 letter country code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
    - `first_doses`: Number of first doses administered
    - `second_doses`: Number of second doses administered
    - `third_doses`: Number of third (booster) doses administered

See `owid-raw.csv` for a file of the required format with the real vaccination campaign data derived from the set provided by [Our World in Data](https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations).
