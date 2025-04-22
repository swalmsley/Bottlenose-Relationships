suppressPackageStartupMessages({
  
  library(data.table)
  library(cmdstanr)
  library(rethinking)
  library(bayesplot)
  library(tarchetypes)
  library(ggplot2)
  library(targets)
  library(rempsyc)
  library(testthat)
  library(brms)
  library(sjPlot)
  library(readxl)
  library(writexl)
  library(asnipe)

  library(dplyr)
  library(ggridges)
  library(spaa)
  library(qgraph)
  library(socmixmods)
  library(iNEXT)
  library(bayestestR)
  library(viridis)
  library(tidybayes)
  library(patchwork)
  library(stringr)
  library(igraph)
  library(scales)   
  library(ggsankey)
  
  # Turn these off for supercomputer
  library(rbbt)
  library(related)
  library(aninet)

  # if installation is required
  # devtools::install_github("samellisq/socmixmods")
  # devtools::install_github("davidsjoberg/ggsankey")
  # devtools::install_github("MNWeiss/aninet")
  
  
})

print('Cleared packages')

