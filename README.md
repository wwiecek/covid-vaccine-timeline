# Vaccines at Velocity

This is a project repository for the following working paper:

> Więcek, Witold, David Johnston, Tomas Dulka, Danny Toomey, and Enlli Lewis. ‘Vaccines at Velocity: Evaluating Potential Lives Saved by Earlier Vaccination in the COVID-19 Pandemic’. medRxiv, 20 June 2023. https://doi.org/10.1101/2023.06.16.23291442.

The last version of the paper is available as [Vaccines at Velocity.pdf]. 

## How to re-generate the results from the paper from scratch

This is an [orderly](https://github.com/vimc/orderly) project. The
directories are:

`archive`: Includes dependencies necessary to generate the model. 

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

`docs`: Includes figures, tables, and legends used in the preprint. 

`figure`: Includes the figures used in the preprint and `report.Rmd`, which can be used to generate the figures.

`src`: Contains structured `orderly` directories which generate the models used in the preprint. A seperate `README` and vignette are included in the `src` directory with directions to add counterfactual scenarios.

The purpose of this repository is to allow others to replicate the analysis used in our preprint and replicate the generation of the preprint in its entirety. 

## Installation

```
    git clone https://github.com/wwiecek/covid-vaccine-timeline.git
    cd covid-vaccine-timeline
    open covid-vaccine-impact-orderly.Rproj
```

## Usage

The models used in the preprint can be generated from scratch by running `run-fits.R`. This operation is very resource intensive and may take several hours to complete. 

Counterfactual scenarios can be generated by running `run-counterfactuals.R`. `run-fits.R` must be ran before running `run-counterfactuals.R` to generate the necessary dependencies. This operation is also resource intensive and takes approximately 1 hour to complete. 

The preprint can be generated locally by running `orderly::orderly_run("preprint")`. This will create a PDF of the preprint at `draft/preprint/[orderly id]/_book/_main.pdf`. The `draft` directory is not included in this repository and will be generated automatically by `orderly`.


## Submission version of the paper

To prepare the journal submission for PLOS we had to implement textual changes using a word processor with tracked changes. This was done by generating a .tex file (`orderly::orderly_run("preprint")` with `keep_tex: true`), manually copying files into the `submission/` folder and then separating out figures from text. Text was converted from .tex to .docx using `pandoc`. Figures and tables were rendered into PDF.




### Common errors

Error while running `run-counterfactuals.R`:
```
Error in `map()`:
ℹ In index: 2.
Caused by error:
! vector memory exhausted (limit reached?)
```

Solution:
Increase the amount of memory that can be allocated to the program. The amount of memory you want to increase the allocation to is up to you, though the authors recommend setting a large limit such as 50gb to avoid this error. This will not impact how R operates outside of the current environment. The following commands are given with the assumption the user wants to increase the memory to 50gb and can be adjusted to a lower size if desired. 
- On Windows, run  `memory.limit(size=50000)`
- On Mac, the solution is a bit more involved. Enter the following commands to create an R environment file (`.Renviron`) and add a maximum memory allocation argument (`R_MAX_VSIZE`): 
    1) Navigate to the `Terminal` tab in RStudio (next to the `Console` in the top left).
    2) Enter `touch .Renviron` to create the environment file.
    3) Enter `open .Renviron` to open the environment file in a terminal window.
    4) Type `R_MAX_VSIZE= 50Gb` to set the memory allocation to 50gb and save the file.
    5) You can verify that the file was correctly saved by re-entering `open .Renviron` and checking for your changes. 
