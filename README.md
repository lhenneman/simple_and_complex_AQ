---
author: "Lucas Henneman"
date: "2/17/2020"
---

## Data and R scripts for reproducing results in "Comparisons of simple and complex methods for quantifying exposure to individual point source air pollution emissions" by Henneman et al. 2020. JESEE.

### Data included in this repository

This repository includes two datasets: `dataset_annual.csv` and `dataset_monthly.csv`. These datasets report population-weighted PM2.5 source impacts (units µg/m3) from each coal-fired power plant in the United States on the entire United States and on seven individual states. Results are reported for 2006 and 2011.

The power plant units are identified by the 'uID' variable, which is a combination of the `Facility ID (ORISPL)` and `Unit ID` fields (seperated by a period) in the EPA AMPD emissions database (https://ampd.epa.gov/ampd/). The annual dataset `dataset_annual.csv` includes the Longitude and Latitude (units degrees) available from the AMPD datababse.

The annual dataset `dataset_annual.csv` includes estimates from the HyADS model ('hyads.pw'), the inverse distance weighted emissions model (IDWE; 'idwe.pw'), and estimates from GEOS-Chem adjoint using five plume rise approximations described in the published manuscript ('initial',	'layers_2-5',	'stack_height',	'stack_height_plus1',	and 'stack_height_plus2'). 

The monthly dataset includes population-weighted estimates from HyADS and IDWE.


### R scripts included in this repository
This repository includes two R scripts: `simple_complex_methods.R` and `functions.R`. `simple_complex_methods.R` includes the primary commands to read the data and create the plots. `functions.R` includes two helper functions that perform analyses and create the plots. 

### Performing the analyses
To run the scripts and recreate plots in the manuscript, 

1. Download all the files to a directory on your local computer. 
2. Edit line 6 in `simple_complex_methods.R` to match your local directory. 
3. Run the remaining commands in `simple_complex_methods.R`





