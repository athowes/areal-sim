library(sf)
library(bsae)
library(ggplot2)
library(dplyr)
library(reshape2)
library(viridis)
library(ar.matrix) # Currently not using
library(tikzDevice)
library(cubature)
library(magrittr)
library(gt)
library(tidyr)
library(sn)
library(purrr)

sapply(list.files("functions/", full.names = TRUE), source)

sf_lightgrey <- "#E6E6E6"
lightgrey <- "#D3D3D3"

lightblue <- "#56B4E9"
lightgreen <- "#009E73"
lightpink <- "#CC79A7"
light_palette <- c(lightblue, lightgreen, lightpink)

midblue <- "#3D9BD0"
midgreen <- "#00855A"
midpink <- "#B3608E"
mid_palette <- c(midblue, midgreen, midpink)

darkblue <- "#004E83"
darkgreen <- "#00380D"
darkpink <- "#802D5B"
dark_palette <- c(darkblue, darkgreen, darkpink)

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

set.seed("01074762")
