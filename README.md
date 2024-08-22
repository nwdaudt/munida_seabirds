# Changing species occurrences in seasonal seabird assemblages at the Subtropical Frontal Zone off southeast Aotearoa New Zealand

This repository hosts code and data from [Daudt *et al.*](https://github.com/nwdaudt/munida_seabirds/tree/main?tab=readme-ov-file#citation) (in review).

A hard copy of this repository is archived at the [Open Science Framework].

***
- Can we define assemblages of seabird species based on water masses at coarse scale (1--100 km)?
- Does seasonality influence assemblages of seabird species off southeast New Zealand?
- Are species' distributions changing over time?

Here, we used a seabird observations undertaken during the Munida Time Series voyages to explore these questions. The 'Munida transect' runs bi-monthly a 60 km offshore transect departing from the Taiaroa Head (Otago Peninsula, Aotearoa New Zealand). The main goal of these voyages is to monitor ocean acidification, including several other biogeochemical markers. It has been going on since 1998, making the Munida transect one of the longest time-series on ocean acidification globally. In addition to its primary goal, the Munida transect allows for collecting other biological and oceanographic samples---including visual surveys of marine megafauna, such as seabirds.

An interesting feature about this transect is that it consistently samples across Neritic, Subtropical Surface and Subantarctic Waters. Very often it crosses the Southland Front, a local expression of the global Subtropical Front. Therefore, using seabird *in situ* observations while carrying out these voyages is an excellent scenario to test whether seabird occurrence responds to water mass even in 'small' scales (i.e. coarse scale, 1--100 km).

The observations are from 2015 to 2023 (nine years), and they consist of the maximum number of individuals counted (at once), within equal-length 5 km 'segments' along the Munida transect.

***

## Project structure

Workspace is set as follows:

```shell
munida_seabirds
├── README.md
├── [./data-raw] ** untracked folder (at the moment) containing raw data
│   ├── ./digitised
│   │  └── Several CSV files with individual surveys
│   ├── ./ebird
│   │  └── AllGLebirdJune2022.csv
│   ├── ./spatial
│   │  ├── munida_5km-transects.gpkg
│   │  ├── nz-coastlines-and-islands-polygons-topo-150k.gpkg
│   │  └── nz_isobath_otago.gpkg
│   ├── ./ts-and-windstress
│   └── NOAA-NCEI_SOI.txt
├── ./data-processed
│   ├── all_data_long.csv
│   ├── seabird_data_long.csv
│   ├── ts_data_summarised_watermasses.csv
│   └── windstress_data_summarised.csv
├── ./renv
├── ./scripts
│   ├── README.md
│   ├── 00_renv-setup.R
│   ├── 01_data-wrangling.R
│   ├── 02_data-wrangling_environmental-data.R
│   ├── 03_data-analysis_EDA.R
│   ├── 04_data-analysis_model-based-ordinations.R
│   ├── 05_data-analysis_univariate-Bernoulli-models.R
│   ├── 06_data-analysis_ENSO.R
│   ├── 07_data-analysis_variogram-spatial-autocorrelation.R
│   └── 08_transect_study-area.R
├── ./results
│   ├── ./variograms
│   └── PDF, CSV, and PNG files with results and EDA analyses
├── ./rmd_tex
│   │  └── ./svg-figs
│   ├── manuscript.Rmd
│   ├── doc_template.docx
│   ├── manuscript.docx
│   ├── manuscript-supplement.doc
│   └── references.bib
└── munida_seabirds.Rproj
```

## How to run

You should be able to reproduce all results using scripts in [`./scripts`](https://github.com/nwdaudt/munida_seabirds/tree/main/scripts) and files from `./data-processed`. The `./scripts/README.md` file will guide you through each step.

### Environment

`R` packages and their dependencies were captured using `{renv} v. 1.0.3` in the [lock.file](https://github.com/nwdaudt/munida_seabirds/blob/main/renv.lock)

However, `{renv}` does *not* capture all the computing environment. The Operational System (OS) and `R` version is detailed below. We did not 'dockerised' our computing environment, so any system dependency you will need to deal with yourself (sorry!).

```shell
R version 4.2.0 (2022-04-22)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.6 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C               LC_TIME=en_NZ.UTF-8        LC_COLLATE=en_NZ.UTF-8
 [5] LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8    LC_PAPER=en_NZ.UTF-8       LC_NAME=C             
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C 
```

A heads up -- you will need `R` **4.2.0** and then make sure to `renv::restore()` to load the project environment with the same package versions as ours (if you are not familiar with `{renv}`, see [their website](https://rstudio.github.io/renv/articles/renv.html)). 

***
## Contributors

[Nicholas W. Daudt](https://github.com/nwdaudt)

Any bugs, suggestions, or enquires, please feel free to contact me or open an issue.

***
## Citation

Please refer to the original paper if using any piece of this repository (code and/or data). This repository is under CC BY 4.0 license.

Daudt, NW; Loh, G; Currie, KI; Schofield, MR; Smith, RO; Woehler, EJ; Bugoni, L; Rayment, WJ. (in review). Changing species occurrences in seasonal seabird assemblages at the Subtropical Frontal Zone off southeast Aotearoa New Zealand.
