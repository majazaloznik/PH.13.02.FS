# prleiminaries
###############################################################################
library(RColorBrewer)
library(dplyr)
library(tidyr)
source("scripts/02-myfunctions.R")
country.codes  <- read.csv("data/country.codes.csv")


## some labeling lookup tables and colour palates

palette7 <- c("lightpink", "chartreuse4", "olivedrab2",
              "skyblue", 
              "orangered1" )
palette4 <- c("lightpink",
              "darkolivegreen3",
              "gold",
              "orchid4")

###############################################################################
## PLOTS
###############################################################################

height = 5.3
width = 10 
par(xpd = TRUE)
layout(matrix(c(1,2), nrow = 1))

FunFinalPlot <- function(i) {
  par(mar = c(2.5,8,0.8, 2))
  FunPlot4(i)
  par(mar = c(2.5,2,0.8, 8))
  FunPlot7(i)
  dev.copy2eps(file=paste0("figures/",country.codes[i,2], ".eps"), height=height, width=width)
}

# ALL 22 PLOTS
lapply(1:22, function(x) FunFinalPlot(x))

###############################################################################
## LEGENDS
###############################################################################
height = 6.5 
width = 10 
# legend 4
layout(matrix(c(1,2), nrow = 1))
par(mar = c(3,4.5,1, 7))
x <- barplot(matrix(rep(c(NA,0.25),4), nrow=4, byrow = TRUE),
             col = palette4, 
             width = c(1,3),
             space = 2,
             axes = FALSE,
             names.arg = c(NA,NA))

rect(rep(par("usr")[1]*1.2, 4), seq(0,.75,.25), rep(par("usr")[2]*0.9, 4), 
     seq(0.25,1,.25),
     border = "white", lwd = 1,
     col =    palette4)

rect(rep(par("usr")[1]*1.2, 4), seq(0,.75,.25), rep(par("usr")[2]*0.9, 4), 
     seq(0.25,1,.25),
     col = "white", density = 20, angle = 30, lwd = 3, border =NA)
barplot(matrix(rep(c(NA,0.25),4), nrow=4, byrow = TRUE),
        col = palette4, 
        width = c(1,3),
        space = 2,
        axes = FALSE,
        names.arg = c(NA,NA),
        add = TRUE)
axis(2, labels = c(5,6,7,8,9,0), at = seq(0,1,0.2), las = 2)
text(rep(par("usr")[2], 4), 0.5*(seq(0,1,0.25) + lag(seq(0,1,0.25)))[2:5], 1:4)

dev.copy2eps(file="figures/legend1.eps", height=height, width=width)

###############################################################################
## legend 5x
layout(matrix(c(1,2), nrow = 1))
par(mar = c(1,4.5,3, 7))
x <- barplot(matrix(rep(c(NA,0.2),5), nrow=5, byrow = TRUE),
             col = palette7, 
             width = c(1,3),
             space = 2,
             axes = FALSE,
             names.arg = c(NA,NA))

rect(rep(par("usr")[1]*1.2, 5), seq(0,.8,.2), rep(par("usr")[2]*0.9, 5), 
     seq(0.2,1,.2),
     border = "white", lwd = 1,
     col =    palette7)

rect(rep(par("usr")[1]*1.2, 5), seq(0,.8,.2), rep(par("usr")[2]*0.9, 5), 
     seq(0.2,1,.2),
     col = "white", density = 20, angle = 30, lwd = 3, border =NA)
barplot(matrix(rep(c(NA,0.2),5), nrow=5, byrow = TRUE),
        col = palette7, 
        width = c(1,3),
        space = 2,
        axes = FALSE,
        names.arg = c(NA,NA),
        add = TRUE)
axis(2, labels = c(5,6,7,8,9,0), at = seq(0,1,0.2), las = 2)
text(rep(par("usr")[2], 5), 0.5*(seq(0,1,0.2) + lag(seq(0,1,0.2)))[2:6], LETTERS[1:5])

text(par("usr")[1]*2, 1.05, "Z")
dev.copy2eps(file="figures/legend2.eps", height=height, width=width)

###############################################################################
# PROPORTION MALE/FEMALE and N total
###############################################################################

i = 2
FunProps <- function(i) {
  df <- read.csv(paste0("data/", country.codes[i,3],".hh.un.csv"))
  df %>%  
    group_by(group) %>% 
    summarise(sum=sum(count)) %>% 
    ungroup() %>% 
    mutate(prop = 2*sum/sum(as.numeric(sum))) %>% 
    unlist() %>% 
    .[c(8,11,12)]
}

props <- sapply(1:22, function(x) FunProps(x))
cbind(country.codes, t(props))

