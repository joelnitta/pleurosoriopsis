# Only run this after making packrat/packrat.lock by
# running install_packages.R

FROM rocker/verse:3.5.3

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update

COPY ./packrat/packrat.lock packrat/

RUN Rscript -e 'install.packages("packrat", repos = "https://cran.rstudio.com/")'

RUN Rscript -e 'packrat::restore()'

# Modify Rprofile.site so R loads packrat library by default
RUN echo '.libPaths("/packrat/lib/x86_64-pc-linux-gnu/3.5.3")' >> /usr/local/lib/R/etc/Rprofile.site

WORKDIR /home/rstudio/
