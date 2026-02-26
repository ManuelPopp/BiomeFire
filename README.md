# BiomeFire

## Biomes
The biome concept was applied following the delineations of Olson et al (2001). More detailled information can be found in [OlsonBiomes.md](https://github.com/ManuelPopp/BiomeFire/blob/main/inf/OlsonBiomes.md).

Table 1: Terrestrial biomes following Olson et al. (2001). University of Maryland (UMD) classes were used as classified by the MODIS Collection 6.1 (C61) Land Cover Type Product (Sulla-Menashe and Friedl, 2022) and are here abbreviated as follows: ENF = Evergreen Needleleaf Forests, EBF = Evergreen Broadleaf Forests, DNF = Deciduous Needleleaf Forests, DBF = Deciduous Broadleaf Forests, MF = Mixed Forests, CS = Closed Shrublands, OS = Open Shrublands, WS = Woody Savannas, S = Savannas, G = Grasslands, B = Barren. Other classes were not used.

| Biome ID | Biome Name                                               | UMD Classes                           |
|----------|----------------------------------------------------------|---------------------------------------|
| 1        | Tropical & Subtropical Moist Broadleaf Forests           | EBF, DBF, MF                          |
| 2        | Tropical & Subtropical Dry Broadleaf Forests             | EBF, DBF, MF                          |
| 3        | Tropical & Subtropical Coniferous Forests                | ENF                                   |
| 4        | Temperate Broadleaf & Mixed Forests                      | DBF, MF                               |
| 5        | Temperate Coniferous Forests                             | ENF, DNF¹                             |
| 6        | Boreal Forests/Taiga                                     | ENF, DNF¹                             |
| 7        | Tropical & Subtropical Grasslands, Savannas & Shrublands | CS, OS, WS, S, G                      |
| 8        | Temperate Grasslands, Savannas & Shrublands              | CS, OS, WS, S, G                      |
| 9        | Flooded Grasslands & Savannas                            | CS, OS, WS, S, G                      |
| 10       | Montane Grasslands & Shrublands                          | CS, OS, G                             |
| 11       | Tundra                                                   | CS, OS, G                             |
| 12       | Mediterranean Forests, Woodlands & Scrub                 | ENF, EBF, DNF, DBF, MF, CD, OS, WS, S |
| 13       | Deserts & Xeric Shrublands                               | CS, OS, B                             |
| 14       | Mangroves²                                               |                                       |

¹Not used to prevent excessive heterogeneity of the data. ²Not analysed.

## References
Olson, David M., Eric Dinerstein, Eric D. Wikramanayake, Neil D. Burgess, George V. N. Powell, Emma C. Underwood, Jennifer A. D’amico, et al. 2001. Terrestrial Ecoregions of the World: A New Map of Life on Earth: A New Global Map of Terrestrial Ecoregions Provides an Innovative Tool for Conserving Biodiversity. *BioScience* 51 (11): 933–38. [10.1641/0006-3568(2001)051[0933:TEOTWA]2.0.CO;2](https://doi.org/10.1641/0006-3568(2001)051[0933:TEOTWA]2.0.CO;2).

Sulla-Menashe, Damien, and Mark A Friedl. 2022. User guide to collection 6 MODIS land cover (MCD12Q1 and MCD12C1) product. https://lpdaac.usgs.gov/documents/1409/MCD12_User_Guide_V61.pdf.

## Notes on the workflow
### Directories
Directories are defined as follows:

- dir_dat Data directory, locally on the machine.
- dir_lud Path to the network drive where large (numbers of) files and intermediate outputs are stored. The network drive is accessible to all machines, laptop as well as compute clusters.
- dir_stc Subdirectory with all static environmental files (climate, landuse, etc)
- dir_ann Subdirectory with all environmental files that differ per year (total annual rainfall, annual NPP, etc).
- dir_fig Local output directory for figures.
- dir_fig_tex Dropbox directory for figures to synchronise directly with Overleaf.

### Trend analysis
#### Data preparation
Run [./rsc/data_preparation/sample_burned_areas.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_preparation/sample_burned_areas.R) for each biome (call the R script with an integer from 1 to 12 as an input argument). This will create the `annual_burned_area.csv` file within each biome folder of the `intermediate_data` subdirectory. Proceed in the same manner with the [./rsc/data_preparation/sample_ba_by_continent.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_preparation/sample_ba_by_continent.R) to compute the `annual_ba_by_continent.csv` files.

#### Data analysis
Once all required intermediate files are generated, run [./rsc/data_analysis/trend_analysis.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_analysis/trend_analysis.R) and [./rsc/data_analysis/trend_analysis_by_continent.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_analysis/trend_analysis_by_continent.R) to generate the figures and tables.

### Binned analysis
#### Data preparation
Run [./rsc/data_preparation/sample_by_bin.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_preparation/sample_by_bin.R) for each biome (providing the biome index as a trailing argument). The script produces `.Rsave` files as output for each biome. If it fails, e.g. due to OOM issues, it can be restarted from the latest checkpoint by calling the script with the `--continue` flag. Each checkpoint resembles a climate bin, not a year, since trends are temporal and, thus, iterations are only done over spatial (climate) subsets.

#### Data analysis
Once all required intermediate files are generated, run [./rsc/data_analysis/binned_trend_analysis.R](https://github.com/ManuelPopp/BiomeFire/blob/main/rsc/data_analysis/binned_trend_analysis.R) to generate figures and tables for both intercept and slope of burned area within climate bins.