FROM rocker/tidyverse:3.5.2

ENV DEBIAN_FRONTEND noninteractive

### Other dependencies for Yang and Smith workflow
# (transdecoder comes with cd-hit-est 4.6)
RUN apt-get update \
### R packages
# drake depends on graph, but graph is only available
# on bioconductor, so install that first
# CRAN packages
&& install2.r -e readxl cowplot ggridges