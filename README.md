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