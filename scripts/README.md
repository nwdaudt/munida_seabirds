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
  - Sets up R environment by calling `renv::restore()`.
* **'01': Tidy up raw data**
  - Standardise and tidy up two sources of data (eBird and the [newly] digitised data);
  - Merge these two [raw] "datasets" into the main one.
* **'02': Environmental data wrangling**
  - Temperature and Salinity (**Figure S1**);
  - Water mass classification;
  - Windstress data (we ended up not using it).
* **'03': Exploratory data analyses**
  - Effort summary (**Table S1**);
  - Percentage of each water mass, for each 5 km segment, by season (**Figure S2**);
  - Number of occurrence per species;
  - Number of samples, by direction for each 5 km segment;
  - Number of species, by direction for each 5 km segment;
  - Number of species and maximum group size, by season (**Figure S3**);
  - Number of species and maximum group size, for each 5 km segment, by season (**Figure 2**);
  - Frequency of occurrence and numeric frequency, by season (**Figure 3**);
  - Frequency of occurrence and numeric frequency, by year (we ended up not using it);
  - Windstress class, by season (we ended up not using it).
* **'04': Multivariate, model-based ordinations (GLLVMs)**
  - Fit null models (**Table S3; Figure S4; Figure 4**);
  - Fit full models (**Table S3**);
  - Compare predictions between GLLVMs accounting for predictors (full models), with and without LVs, and raw data [just another layer of validation, not presented in the manuscript];
  - Covariate selection based on best full model (**Table S3**);
  - Compare predictions between GLLVMs '~ distcoast', '~ distcoast + season', and raw data [just another layer of validation, not presented in the manuscript]  (**Figure S5**);
  - Coefficient plots for the chosen best model including predictors (**Figure 5**).
* **'05': Univariate, Binomial GLMM**
  - Fit GLMMs;
  - Nicely plot the coefficient and confidence intervals for Year for each species (**Figure 6**).

The following scripts were part of exploratory data analysis and were not officially reported in the manuscript (dotted-line diamonds, in the schematic figure above). **Note**: the data is not ideal to run these analyses, as briefly discussed under 'Limitations of the study' in the manuscript.

* **'06': ENSO**
  - Summary of ENSO phases per year and voyages (**Figure S6**).
* **'07': Variograms**
  - Spatial correlation among seabird observations for each species (check **`./results/variograms/`**).

Script **'08'** is just an auxiliary script for exporting each of the **Figure 1** (study area) individual panels.

***

**Figures 1, 4 and 5** were tiled together outside R (using [Inkscape](https://inkscape.org/) software), and were stored in `./rmd_tex/svg-figs/`. The (sub-)figures are all individual files stored at `./results/`. 

**Figure 1** in the manuscript were built from script 08, and then edited in Inkscape. 

***

## Manuscript

After running the above scripts, you will get all the results needed to compile the manuscript. The source file for the manuscript is at `./rmd_tex/TeX/ms-ECSS.tex`. **Note:** to avoid repetition of files I have **not** copied all Figs to this directory (as Elsevier requests that figures are in the same directory as the TeX file) -- so you may need to do it yourself if you want to compile the exact PDF.
