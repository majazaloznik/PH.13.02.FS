###############################################################################
## 0. preliminaries 
## 1. combine all summaries in one table 
###############################################################################

## 0. preliminaries 
###############################################################################
library(tidyverse)
country.codes <- read.csv("data/country.codes.csv")


## 1. combine all summaries in one table 
###############################################################################

df <- data.frame(NULL)
for(i in 1:nrow(country.codes)) {
  x.gen <- read.csv(paste0("data/", country.codes[i,2],".hh.gen.csv"))
  # also collapse the 4 and 5 generation households here:
  hh.7gen <- data.frame(code = c(1,2,3,4,4,4,5),
                        hh.type = c("single",  "one gen", "two gen", "three gen",
                                "four gen", "five gen", "skipped"))
  hh.5gen <- data.frame(code = c(1,2,3,4,5),
                        hh.type = factor(1:5, labels = c("single",  "one gen", "two gen", "three+ gen",
                                 "skipped")))
    x.gen %>% 
    left_join(hh.7gen) %>% 
    arrange(code) %>% 
    group_by(code, group) %>% 
      summarise_if(is.numeric, sum) %>% 
      left_join(hh.5gen) %>%
      ungroup() %>% 
      select(-code) %>% 
      arrange(group, hh.type) -> x.gen

  x.un <- read.csv(paste0("data/", country.codes[i,2],".hh.un.csv"))  
  x.un <- rename(x.un, hh.type = hh.type4)
  x.gen <- cbind(x.gen, typology = "inter")
  x.un <- cbind(x.un, typology = "un")
  x <- rbind(x.gen, x.un)
  x <- cbind(x, country.codes[i,])
  df <- rbind(df, x)
}

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

FunPlotHalf <- function(cntry = "UnitedStates", typ = "un", grp= "old", pos = "right") {
  s.df <- filter(df,
                 country == cntry &
                   typology == typ) 
  
  sumz <- s.df %>% 
    group_by(group) %>% 
    summarise(count = sum(count)) %>% 
    filter(group != "total") %>% 
    mutate(prop = count/sum(count))
  
  width <- if(grp == "old") sumz$prop[2:1] else sumz$prop[4:3]
  
  bars.back <- s.df %>% 
    filter(group == "total") %>% 
    pull(prop) %>% 
    cumsum %>% 
    c(0,.)
  
  bars.front <-   s.df %>% 
    filter(grepl(grp, group)) %>% 
    select(group, prop, hh.type) %>% 
    spread(group, prop) %>% 
    .[3:2] %>%  
    as.matrix()
   
  pal <- switch(typ,
                inter = paletteInter,
                un = paletteUN)
 # start plot
  switch(pos,
         left =  par(mar = c(3.5,8,0.8, 2)),
         right = par(mar = c(3.5,2,0.8, 8)))
  
  x<-barplot(bars.front,
             col = pal, 
             width = width,
             space = 1.3, 
             axes = FALSE,
             names.arg = c(NA,NA))
  text(x, y=-0.03, c("Men", "Women"))
  text(x, y=-0.1, c( paste(format(width[1]*100, digits = 4), "%"),
                            paste(format(width[2]*100, digits = 4), "%")))
       
  rect(rep(par("usr")[1]*1.2, 5), bars.back[1:5], 
       rep(par("usr")[2]*0.9, 5), 
       bars.back[2:6],
       border = "white", lwd = 1,
       col =    pal)
  
  rect(rep(par("usr")[1]*1.2, 5), bars.back[1:5], 
       rep(par("usr")[2]*0.9, 5), 
       bars.back[2:6],
       col = "white", density = 15, angle = 30, lwd = 3, border =NA)
  
  barplot(bars.front,
          col = pal, 
          width = width,
          space = 1.3, 
          axes = FALSE,
          names.arg = c(NA,NA),
          add = TRUE)
 switch(pos,
        left = axis(4, las = 2),
        right = axis(2, labels = rep(NA,6)))

}

FunPlotHalf(cntry = "Belarus",
            grp = "old",
            typ = "inter",
            pos = "left")
FunPlotHalf(cntry = "Belarus",
            grp = "old",
            typ = "un",
            pos = "right")

    midpoints <- (cumsum(pull(bars7,3)) - lag(cumsum(pull(bars7,3)), default = 0))/2 + 
    lag(cumsum(pull(bars7,3)), default = 0)
  
  text(rep(par("usr")[2], 5), midpoints7, letters[1:5])

