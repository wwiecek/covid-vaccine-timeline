
<!-- README.md is generated from README.Rmd. Please edit that file -->

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
