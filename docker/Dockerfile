FROM rocker/tidyverse:3.5.1

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
# Github R packages
&& Rscript -e 'devtools::install_github("thomasp85/patchwork")' \
           -e 'devtools::install_github("rstudio/gt")' \
# CRAN R packages
&& install2.r -e readxl cowplot ggridges here viridis conflicted
