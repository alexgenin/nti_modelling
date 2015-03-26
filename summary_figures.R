#!/usr/bin/R -f 
# 
# Code generating the graphs for summary.tex
# 
# Full rockyshore model.
#

library(deSolve)
library(magrittr)
library(ggplot2)
library(plyr)
library(devtools)
library(tidyr)
library(rollply) # for grid-building functions

# Reload functions and data from dev folder
oldpwd <- getwd()
OUTDIR <- paste0(getwd(),"/res")

# Set narrow width as kile has a narrow terminal window
options(width=60)

export_plot <- function(plot.obj, filename, width, height) { 
  ggsave(filename=paste0(OUTDIR,"/",filename),
         plot=plot.obj, 
         width=width,
         height=height)
}

# devdir
devdir <- "../../../dev"
setwd(devdir)
document()

# 
# 4 producers
# 2 intermediate consumers
# 2 top predators
# 

for (file in list.files(paste0(oldpwd,"/R"), full.names=TRUE)) {
  message("Sourcing", file)
  source(file)
}
setwd(oldpwd)
