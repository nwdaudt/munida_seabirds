# Changing species occurrences in seasonal seabird assemblages at the Subtropical Frontal Zone off southeast Aotearoa New Zealand

This `.scripts/README` guide you through the scripts used

## General workflow and format

The scripts are named following a numerical order, so its is easy to follow (numbers relate to script names):

![Fig 1. Scripts workflow](https://github.com/nwdaudt/munida_seabirds/blob/main/scripts/scripts-workflow.png)

All scripts were developed using [RStudio IDE](https://posit.co/products/open-source/rstudio/) and, for the sake of facility, I structured them into 'code sections' (`####`). So, if you use RStudio, you can easily follow the *document outline*.

Below, I provide a brief description underlying what each script does.

***

These were the main scripts used for data wrangling and analyses (solid-line squares, in the figure above).

* **'00': R environment**
  - Sets up R environment by calling `renv::restore()`
* **'01': Tidy up raw data**
  - Standardise and tidy up two sources of data (eBird and the [newly] digitised data)
  - Merge these two [raw] "datasets" into the main one
* **'02': Environmental data wrangling**
  - Temperature and Salinity
  - Water mass classification
  - *Windstress data (ended up not using it)*
* **'03': Exploratory data analyses**
  - Effort summary (**Table S1**)
  - Number of occurrence per species
  - Number of samples, by direction /5 km segment
  - Number of species, by direction /5 km segment
  - Number of species and total number of birds, by season (**Figure S2**)
  - Number of species and total number of birds, /5 km segment, by season (**Figure 2**)
  - Frequency of occurrence and numeric frequency, by season (**Figure 3**)
  - *Frequency of occurrence and numeric frequency, by year (ended up not using it)*
  - Percentage of each water mass, /5 km segment, by season (**Figure S1**)
  - *Windstress class, by season (ended up not using it)*
* **'04': Multivariate, model-based ordinations (GLLVMs)**
  - Null models (**Figure 4; Figure S3**)
  - Full models
  - Predictor (covariate) selection (**Figure S4**)
  - Coefficient plots (**Figure 5**)
  - *Compare predictions based on the best full model with 0, 1 and 2 Latent Variables vs. raw data* [just another layer of validation, but I did not present it in the manuscript]
* **'05': Univariate, Binomial GLMs**
  - Yearly probability of occurrence, conditional to distance from coast (**Figure 6, 7**)
  - Seasonal probability of occurrence, conditional to distance from coast (**Figure S5**)

The following scripts were part of exploratory data analysis and were not officially reported in the manuscript (dotted-line diamonds, in the figure above).

* **'06': ENSO**
  - Summary of ENSO phases per year and voyages (**Figure S6**)
* **'07': Variograms**
  - Spatial correlation among seabird observations for each species
  - **Note**: the data is not ideal to run this analysis, as briefly discussed in the manuscript.

Script **'08'** is just an auxiliary script for exporting each of the **Figure 1** (study area) individual panels.

***

Some figures were tiled together outside R (using [Inkscape](https://inkscape.org/) software), and were stored in `./rmd_tex/svg-figs/`. The (sub-)figures are all individual files from/in `./results/`. 

**Figure 1** in the manuscript were built from script 08, and then edited in Inkscape. 

***

## Manuscript

After running the above scripts, you will get all the results needed to compile the manuscript. The source `RMarkdown` file is in  `./rmd_tex/`. 

