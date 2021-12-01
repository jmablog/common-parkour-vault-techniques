# Common parkour vault techniques, landing styles, and their effects on landing forces

[![DOI](https://zenodo.org/badge/425025212.svg)](https://zenodo.org/badge/latestdoi/425025212)

This repository is all the code, analysis, and text that goes into building my undergraduate dissertation, "Common parkour vault techniques, landing styles, and their effects on landing forces", available to read online here: https://jmablog.com/research/pkvs/

This is a [Bookdown](https://bookdown.org/) project, in [R](https://www.r-project.org/).

## Steps for reproducing from source with R

This project was built with R version 4.0.2.

- Clone this directory to your local machine.
- If using Rstudio, open the .Rproj file in the base project directory. Otherwise, set the working directory to the base project directory in R. If not using Rstudio, you may need to also manually install [Pandoc](https://pandoc.org/).
- If not already installed, install the [renv](https://rstudio.github.io/renv/) and [devtools](https://devtools.r-lib.org/) R packages. See those package sites for installation instructions.
- Open the file `create_report.R` for the stages required to build the report, as detailed below:
  - Run `renv::activate()` followed by `renv::restore()` to install all the required packages used in this project in a local renv directory. This should not impact your regular R package library, but may take a little while.
  - Run `devtools::load_all()` to load the custom functions I have written for this project.
  - Run `build_book()` to build the book into the folder `book/builds/{date}`. `build_book`accepts the following arguments:
    - `format`: string, default "print". One of "all", "print", "web", "gitbook", "pdf", or "word". "all" produces gitbook, pdf, and word outputs. "print" produces just pdf and word outputs. "web" produces gitbook and pdf outputs. Individual output selections produces just that output.
    - `word_num`: boolean, default FALSE. Set if Word output should have numbered sections or not.

**Note:** The original work was written targeting PDF and gitbook output, so Word output may be broken or messy.

## About

The main text is in `book/src/common-parkour-vault-techniques.Rmd`. Add-in text is also in the `src` folder, marked by filenames beginning with an underscore, used to slightly alter the text when building to gitbook rather than PDF.

References are stored as BibTeX in `book/bib/references.bib`. This project was written using Cite Them Right 10th Edition reference formatting; a CSL file for this format is included in `book/bib`.

All main analysis code is in the `tables.R` and `plots.R` files in the `analysis` folder, broken into knitr code chunks with `## ----` headers. This code is then imported into and run inside the main text on build using the [knitr child chunk option](https://yihui.org/knitr/options/#child-documents).

Other functions for use in the build are in the `R` folder and some other statistical analysis that didn't end up directly reported in the main text is also in the `analysis` folder.

Formatting is mainly controlled by files in the `book/assets` folder, with some custom CSS for gitbook output, some custom LaTeX for PDF output, and custom .docx templates for Word output. All are tweaked by settings contained in their respective .yaml files in `book/assets/output_yml`, which is then used as the base for Bookdown's `_output.yml` options file on build with `build_book`.

## Data

The data folder contains all the data in a variety of formats, including the raw force curves for all participants in both .txt and .dat formats. The .dat files are the force profile files saved by [Bioware](https://www.kistler.com/en/product/type-2812a/) software directly, while the .txt files are the exported equivalent, containing some metadata at the top of the file. 

The cleaned and compiled data can also be found in Rdata, Excel, CSV and SPSS formats. Some formats also include the data in long or wide layouts, as preferred. Most variables in each format should be self explanatory, with the most important being the distinction between the force values being in Newtons (result\_n) or in multiples of participant bodyweight (result\_bw).

**Please be aware**: The compiled datasets (in the 'processed' folder and usually anything with 'pkvs' in the filename) for R/Excel/CSV/SPSS etc have been through a cleaning and import script, found in `analysis/_data.R` and further modified in the `index.Rmd` setup chunk. Mainly this affects the Precision Landing style figures, as these are **halved** to approximate the force through a single limb, in order to match the Running Landing style figures. The raw data (.txt and .dat files) contains the **original unhalved** figures for Precision Landings, if desired.

## Why are there so many file versions of the processed data?

Because I taught myself R to do this project, and went through a phase of experimenting with how it interacts with different file formats. Since I had them, I thought I might as well put them out there. They’re all the same underlying data; just pick whichever you prefer, or work from the raw files to start from scratch. 
