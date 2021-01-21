# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/geospatial:4.0.0

# required
MAINTAINER Li-Ying Wang <jaybin502@gmail.com>

COPY . /kwl-burials

# go into the repo directory
RUN . /etc/environment \
  # build this compendium package
  && R -e "remotes::install_github('r-lib/remotes')" \
  && R -e "devtools::install('/kwl-burials', dep=TRUE)" \
  && R -e "remotes::install_github('benmarwick/wordcountaddin', type = 'source', dependencies=TRUE)" \
  && R -e "remotes::install_github('benmarwick/rrtools', type = 'source', dependencies=TRUE)" \
  && R -e "remotes::install_github('3wen/legendMap', type = 'source', dependencies=TRUE)" \
  && R -e "remotes::install_github('nevrome/ggpointgrid', type = 'source', dependencies=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/kwl-burials/analysis/paper/paper.Rmd')"
