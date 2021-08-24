
# kwlburials

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/LiYingWang/kwl-burials/master?urlpath=rstudio) [![run-docker.yaml](https://github.com/LiYingWang/kwl-burials/workflows/.github/workflows/run-docker.yaml/badge.svg)](https://github.com/LiYingWang/kwl-burials/actions)


This repository contains the data and code for our paper:

> Li-Ying Wang & Ben Marwick, (2021). A Bayesian networks approach to infer social changes from burials in northeastern Taiwan during the European colonization period. *Journal of Archaeological Science 134*
> <https://doi.org/10.1016/j.jas.2021.105471>

Our pre-print is online here:

> Li-Ying Wang & Ben Marwick, (2021). A Bayesian networks approach to infer social changes from burials in northeastern Taiwan during the European colonization period. *Journal of Archaeological Science 134*
> <https://osf.io/preprints/socarxiv/3vfea/>

### How to cite

Please cite this compendium as:

> Li-Ying Wang & Ben Marwick, (2021). Compendium of R code and data for "A Bayesian networks approach to infer social changes from burials in northeastern Taiwan during the European colonization period". Accessed 23 Aug 2021. Online at
> <https://osf.io/xga6n/>

### Contents

The most important parts of this compendium, for most users, are:

  - [:file\_folder: analysis/code](analysis/code): R script files that include code to analyses data and reproduce the results and figures in this paper 
  - [:file\_folder: analysis/paper](analysis/paper): R Markdown files for generating the paper and supplementary materials
  - [:file\_folder: analysis/data](analysis/data): Data used in the analysis, including raw data and derived data
  - [:file\_folder: analysis/figures](analysis/figures): Figures that were created by R script files and included in the paper 
  
The key parts of the code in this project are listed below:


```
analysis
   |-- code
   |   |-- 000-Kiwulan-map.R
   |   |   # makes Fig 1: the location of Kiwulan
   |   |-- 000-prep-data.R
   |   |   # imports and inspects data 
   |   |   # makes Fig 3: the distribution of trade beads 
   |   |-- 001-data-tidy.R
   |   |   # tidies data for network construction 
   |   |-- 002-burial-location.R
   |   |   # makes Fig 2: the location of burials by periods
   |   |-- 003-burials-pre-network-modelling.R
   |   |   # build a Bayesian model for the pre-European network
   |   |-- 004-burials-post-network-modelling.R
   |   |   # build a Bayesian model for the post-European network
   |   |-- 005-network-diagrams-two-phases.R
   |   |   # makes Fig 4: burial network diagrams for both periods
   |   |-- 006-bergm-distribution-stats.R
   |   |   # makes Fig 5: posterior density estimates for the ergm parameters 
   |   |-- 007-bgof-assessment-vis.R
   |   |   # makes Fig 7: distribution moments for the observed and simulated distributions 
   |   |   # makes Fig 3 in the SI: Goodness-of-fit diagnostics for the pre-European model
   |   |   # makes Fig 4 in the SI: Goodness-of-fit diagnostics for the post-European model
   |   |-- 008-bootstrap-CI.R
   |   |   # makes Fig 6: results of the vertex bootstrap analysis
   |   |-- 009-bootstrap-t-test.R
   |   |   # test for differences between network statistics
   |   |-- 999-bgof-custom-function.R
           # customizes bgof function to adjust Fig 3 & 4 in the SI
```

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/parkgayoung/racisminarchy/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses rocker-project.org
Docker images to ensure a consistent and reproducible computational
environment. These Docker images can also be used locally.

You can install this compendium as an R package, kwlburials, from
GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("LiYingWang/kwlburials")
```

Or You can download the compendium as a zip file from this URL:
[master.zip](https://github.com/LiYingWang/kwl-burials/archive/master.zip). After unzipping:  
- open the `.Rproj` file in RStudio, this will open our project in
RStudio on your computer  
- run `renv::restore()` to ensure you have the packages this analysis
depends on (also listed in the [DESCRIPTION](/DESCRIPTION) file).  
- run the R code that produces the figures and numerical results presented
in the paper, and generate our manuscript by rendering our R Markdown
document into a Microsoft Word document.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
