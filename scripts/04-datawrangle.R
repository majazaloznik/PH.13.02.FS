###############################################################################
## 0. preliminaries ###########################################################
## 1. plot ####################################################################
###############################################################################

## 0. preliminaries ###########################################################
library(tidyr)
library(dplyr)
source("scripts/02-myfunctions.R")
df <- read.csv("data/summaries.csv")
###############################################################################
## plot 

layout(matrix(c(1,2), nrow = 1))
## Half Plot function
paletteInter <- c("lightpink", "chartreuse4", "olivedrab2",
              "skyblue", 
              "orangered1" )
paletteUN <- c("lightpink",
              "darkolivegreen3",
              "gold",
              "orchid4")
par(xpd = TRUE)

FunPlotHalf(cntry = "Belarus",
            grp = "old",
            typ = "un",
            pos = "left")
FunPlotHalf(cntry = "Belarus",
            grp = "old",
            typ = "inter",
            pos = "right")


