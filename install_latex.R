# Install latex packages using tinytex

# This would happen automatically anyways when rendering the Rmd to pdf, 
# but requires downloading packages and may not work during updates of Tex Live. 
# Better to install to the docker image once and keep them there.

tinytex::tlmgr_update()

latex_packages <- c(
  "amsmath",
  "latex-amsmath-dev",
  "iftex",
  "mathspec",
  "etoolbox",
  "euenc",
  "fontspec",
  "tipa",
  "xunicode",
  "geometry",
  "auxhook",
  "bigintcalc",
  "bitset",
  "etexcmds",
  "gettitlestring",
  "hycolor",
  "hyperref",
  "intcalc",
  "kvdefinekeys",
  "kvsetkeys",
  "letltxmacro",
  "ltxcmds",
  "pdfescape",
  "refcount",
  "rerunfilecheck",
  "stringenc",
  "uniquecounter",
  "zapfding",
  "pdftexcmds",
  "infwarerr",
  "kvoptions",
  "grffile",
  "titling",
  "booktabs",
  "caption"
)

tinytex::tlmgr_install(latex_packages)
