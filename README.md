# DogPopSim
R code for deterministic mathematical population model of free-roaming dog populations and sterilisation interventions.

# Free-roaming dog sterilisation strategy model

This repository contains R code for a deterministic simulation model evaluating sterilisation strategies for free-roaming dog populations. The model was developed to compare the population impact and cost-efficiency of different sterilisation approaches, including variation in intervention duration, interval, intensity, sex targeting, ownership/roaming status, and catch-return-guided stopping rules.

The code accompanies the paper:

**Fielding H. et al. *****Modelling sterilisation strategies to maximise population impact and cost-efficiency in free-roaming dog populations.***** [Journal/preprint details to be added].**

## Overview

Free-roaming dog population management is important for animal welfare, public health, rabies control, and reducing human-dog conflict. Surgical sterilisation is widely used, but there is limited evidence on how best to allocate resources across time, space, and target groups.

This model simulates a free-roaming dog population using daily time steps. Dogs are divided into human-dependent/owned and independent/stray subpopulations, with age-, sex-, and reproductive-state compartments. The model is used to explore how different sterilisation strategies affect population size, sterilisation coverage, and implementation costs.

## Model structure

The model includes:

* human-dependent/owned and independent/stray free-roaming dog subpopulations;
* pup, juvenile, and adult life stages;
* male and female compartments;
* adult female reproductive states: anoestrus, heat, pregnant, non-pregnant, and neutered;
* births, mortality, and movement from dependent to independent compartments;
* seasonal reproduction;
* density-dependent stray mortality;
* abandonment/turf-out from owned to stray compartments;
* female-only and male-inclusive sterilisation strategies;
* fixed-duration and repeated sterilisation campaigns;
* catch-return-guided strategies, where catching effort can change in response to previous catch numbers;
* intervention cost calculations.

## Requirements

The model is written in R.

Main R packages used include:

```r
dplyr
tidyr
ggplot2
scales
tidyverse # (for plots)
```

## Running the model

To run the model, first ensure that the required input parameter files are available in the expected location, for example:

```r
source("input/SelectedParameters_20210420.R")
```

Then source the model function and run a sterilisation scenario, for example:

```r
source("R/speyFun.R")

neuterDF <- speyFun(
  nYears = 17,
  speying = TRUE,
  speyingR = TRUE,
  speyingC = TRUE,
  speyingMales = TRUE,
  speyingStart = 1,
  speyingInterval = 365,
  speyingDuration = 90,
  speyedPerDay = 25,
  numTeams = 3,
  minCatch = 5,
  neuterBreak = 30
)
```

The returned object is a data frame with one row per simulated day. Outputs include compartment sizes, births, daily neutering numbers, mortality rates, abandonment/turf-out rates, intervention settings, and population totals.

**makeStrategyDF.R** creates a dataframe of multiple sterilisation strategy combinations
to enter into **RunModelStrategy.R** which runs speyFun multiple times (for use with HPC) 
The outputs should be saved and **OutputFunctions.R** code can be used to summarise and 
visualise the outputs of the model simulations.

## Key model outputs

The model can be used to estimate:

* total owned and stray dog population size over time;
* adult and juvenile population sizes;
* numbers of pups born;
* female sterilisation coverage;
* male sterilisation coverage;
* numbers of females and males sterilised per day;
* population reduction under different strategies;
* relative cost-efficiency of alternative sterilisation strategies (in output functions).

## Web application
A shiny web application 'DogPopSim' is available at https://field.shinyapps.io/DogPopSimApp and allows visualisation of the sterilisation strategies simulated in the associated paper. R code for the app is in this repository: **app.R**

## Citation

If you use this code, model structure, or outputs in academic work, please cite the associated paper:

> Fielding H. et al. *Modelling sterilisation strategies to maximise population impact and cost-efficiency in free-roaming dog populations.* [Journal/preprint details, year, DOI to be added].

Please also cite this repository:

> Fielding H. [2026]. *DogpopSim.* GitHub. [Repository URL or DOI to be added].

## Licence

This code is released under the **GNU General Public License v3.0**.

You are free to use, modify, and redistribute the code under the terms of the GPL-3.0 licence. Any redistributed modified versions must also be made available under the GPL-3.0, and copyright and licence notices must be retained.

Please note that GPL-3.0 does not prohibit commercial use. However, it does require that redistributed derivative software remains open under the same licence.

## Acknowledgement request

If you use or adapt this code, please acknowledge the original authors and cite the associated paper and repository. This helps make the work discoverable and supports transparent reuse of scientific models.

Suggested acknowledgement:

> This work used code from Fielding et al.’s free-roaming dog sterilisation strategy model, developed for the study *Modelling sterilisation strategies to maximise population impact and cost-efficiency in free-roaming dog populations.*

## Disclaimer

This model is provided for research and educational purposes. It is a simplified representation of free-roaming dog population dynamics and sterilisation interventions. Outputs depend on the parameter values, assumptions, and intervention scenarios used. Users should review the model assumptions carefully before applying the code to new settings or policy decisions.

## Contact

For questions about the model, please contact:

**Helen Fielding**
helen.fielding@ed.ac.uk
University of Edinburgh
