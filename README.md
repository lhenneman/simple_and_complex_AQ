---
author: "Lucas Henneman"
date: "2/17/2020"
---

# Data and R scripts for reproducing results in "Comparisons of simple and complex methods for quantifying exposure to individual point source air pollution emissions" by Henneman et al. 2020. JESEE.

## Data included in this repository

This repository includes two dataset: `dataset_annual.csv` and `dataset_monthly.csv`. These datasets report  population-weighted PM2.5 source impacts from each coal-fired power plant in the United States on the entire United States and on seven individual states. Results are reported for 2006 and 2011.

The annual dataset includes estimates from the HyADS model ('hyads.pw'), the inverse distance weighted emissions model (IDWE; 'idwe.pw'), and estimates from GEOS-Chem adjoint using five plume rise approximations described in the published manuscript ('initial',	'layers_2-5',	'stack_height',	'stack_height_plus1',	and 'stack_height_plus2'). 

The monthly dataset includes estimates from HyADS and IDWE.


## R scripts included in this repository

