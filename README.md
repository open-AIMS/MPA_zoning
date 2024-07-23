MPA Zoning analyses (fish)
==============================

Zoning analyses (Green vs Blue), Sector/Year combination.
Responses:
  - HC    : group summary table
  - SC    : group summary table
  - A     : group summary table
  - Total abundance         : fish05 (family)
  - Large fish abundance    : fish05 (family)
  - Pomacentridae abundance : fish05 (family)
  - Coral trout density     : fish05 (species code)
  - Trout Biomass           : fish05 (species code)
  - Secondary Target abundance : fish05 (family)
  - Herbivore                  : fish05 (family)
Output:
  data.frame: cellmeans etc
  figures: individual sectors

Prior to running please review the following
- parameters/density_table.csv
- parameters/L-W co-eff.csv
- parameters/MPA_paper trophic groups.csv
- parameters/webGroups.csv
- parameters/model.settings.txt

There are two broad purposes for these scripts.
 - `purpose='Reports'`
   This is to generate a Report on MPA Zoning every two years
 - `purpose='Web'`
   This is to generate Sector specific outputs for webpages
   As such, there is a global variable (sector) that indicates
   which sector to model.
   
`sector` can either be the name of an actual sector or `'All'` in
    which case, all sectors will be modelled in a single model

Directory structure
```
\
|- data
|  |- primary
|  |- processed
|  |- modelled
|- outputs
|  |- figures
|  |- tables
|- R
|  |- helperFunctions.R
|  |- 00_main.R
|- scripts
|  |- dbExport.jar
|- parameters
|  |- 
```
