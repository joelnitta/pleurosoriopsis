FROM rocker/verse:4.2.1

# Install R packages with renv ----

# Create directory for renv project library
RUN mkdir /renv

# Modify Rprofile.site so renv uses /renv for project library
RUN echo 'Sys.setenv(RENV_PATHS_LIBRARY = "/renv")' >> /usr/local/lib/R/etc/Rprofile.site

# Initialize a 'dummy' project and restore the renv library.
# Since the library path is specified as above, the library will be restored to /renv
RUN mkdir /tmp/project

COPY ./renv.lock /tmp/project

WORKDIR /tmp/project

# Turn off cache
RUN Rscript -e 'install.packages("renv"); renv::consent(provided = TRUE); renv::settings$use.cache(FALSE); renv::init(bare = TRUE); renv::restore()'

# Install latex packages with tinytex ----

COPY install_latex.R .

RUN Rscript install_latex.R

# Add script to clone repo and run analysis

COPY make.sh /tmp/.

# Default command is to clone repo and run analysis
CMD ["bash", "/tmp/make.sh"]