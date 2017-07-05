# prleiminaries
###############################################################################
library(RColorBrewer)
source("scripts/02-myfunctions.R")
country.codes  <- read.csv("data/country.codes.csv")


## some labeling lookup tables and colour palates

palette7 <- c("lightpink", "chartreuse4", "olivedrab2",
              "steelblue1", #"violetred3",
              "orangered1" )
palette4 <- c("lightpink",
              "springgreen3",
              "yellow",
              "goldenrod1")

###############################################################################

height = 5.5 
width = 10 
layout(matrix(c(1,2), nrow = 1))

FunFinalPlot <- function(i) {
  par(mar = c(2,8,0.8, 2))
  FunPlot4(i)
  par(mar = c(2,2,0.8, 8))
  FunPlot7(i)
  dev.copy2eps(file=paste0("figures/",country.codes[i,2], ".eps"), height=height, width=width)
}

FunFinalPlot(2)
lapply(1:22, function(x) FunFinalPlot(x))


