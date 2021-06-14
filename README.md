
# kwlburials

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/LiYingWang/kwl-burials/master?urlpath=rstudio) [![run-docker.yaml](https://github.com/LiYingWang/kwl-burials/workflows/.github/workflows/run-docker.yaml/badge.svg)](https://github.com/LiYingWang/kwl-burials/actions)


This repository contains the data and code for our paper:

> Li-Ying Wang & Ben Marwick, (2021). A Bayesian networks approach to infer social changes in burials in northeastern Taiwan during the European colonization period. 

Our pre-print is online here:

> Li-Ying Wang & Ben Marwick, (2021). A Bayesian networks approach to infer social changes in burials in northeastern Taiwan during the European colonization period. 
> Accessed 16 Mar 2021. Online at
> <https://osf.io/preprints/socarxiv/3vfea/>

### How to cite

Please cite this compendium as:

> Li-Ying Wang & Ben Marwick, (2021). Compendium of R code and data for "A Bayesian networks approach to infer social changes in burials in northeastern Taiwan during the European colonization period". Accessed 16 Mar 2021. Online at
> <https://osf.io/xga6n/>

### Contents

This repository contains:

  - [:file\_folder: analysis/code](analysis/code): R script files that include code to analyses data and reproduce the results and figures in this paper 
  - [:file\_folder: analysis/paper](analysis/paper): R Markdown files for generating the paper and supplementary materials
  - [:file\_folder: analysis/data](analysis/data): Data used in the analysis, including raw data and derived data
  - [:file\_folder: analysis/figures](analysis/figures): Figures that were created by R script files and included in the paper 
  
The key parts of the code in this project are listed below:


```
analysis
   |-- code
   |   |-- 000-Kiwulan-map.R
   |   |   # makes the regional map
   |   |-- 000-prep-data.R
   |   |   # import data and ...
   |   |-- 001-data-tidy.R
   |   |   # import data and ...
   |   |-- 002-burial-location.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 003-burials-pre-network-modelling.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 004-burials-post-network-modelling.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 005-network-diagrams-two-phases.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 006-bergm-distribution-stats.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 007-bgof-assessment-vis.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 008-bootstrap-CI.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 009-bootstrap-t-test.R
   |   |   # makes Fig X: [caption snippet]
   |   |-- 999-bgof-custom-function.R
           # makes Fig X: [caption snippet]
```

### How to download or install

You can download the compendium as a zip from from this URL:
</archive/master.zip>

Or you can install this compendium as an R package, kwlburials, from
GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("LiYingWang/kwlburials")
```

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
