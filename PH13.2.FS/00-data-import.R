## ============================================================================
## ==== 00. data import =======================================================
## ==== 01. variable definitions ==============================================
## ==== 02. function definitions ==============================================
## ============================================================================

## ==== 00. data import =======================================================

fs_deposit_id <- 5415055
deposit_details <- fs_details(fs_deposit_id)
deposit_details <- unlist(deposit_details$files)
df <- read.csv(deposit_details[1])
df$hh.type <- factor(df$hh.type,levels(df$hh.type)[c(5,4,8,7,6,3,2,1)])

## ==== 01. variable definitions ==============================================

paletteInter <- c("lightpink", "chartreuse4", "olivedrab2",
                  "skyblue", 
                  "orangered1" )
paletteUN <- c("lightpink",
               "darkolivegreen3",
               "gold",
               "orchid4")


## ==== 02. function definitions ==============================================

FunPlotHalf <- function(cntry = "Armenia", 
                        typ = "un", 
                        grp= "old", 
                        pos = "right") {
  s.df <- filter(df,
                 country == cntry &
                   typology == typ) 
  
  sumz <- s.df %>% 
    group_by(group) %>% 
    summarise(count = sum(count)) %>% 
    filter(group != "total") %>% 
    mutate(prop = count/sum(count))
  
  width <- if(grp == "old") sumz$prop[2:1] else sumz$prop[4:3]
  
  s.df %>% 
    filter(grepl(grp, group) | group == "total") %>% 
    select(group, prop, hh.type, country, typology) %>% 
    spread(group, prop) %>% 
    arrange(hh.type) -> s.df
  
  bars.back <- s.df %>% 
    pull(total) %>% 
    cumsum() %>% 
    c(0,.)
  
  bars.front <- s.df  %>% 
    .[5:4] %>%  
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
  side.y <- switch(pos,
                   right = bars.front[,2],
                   left = bars.front[,1])
  side.y <- (cumsum(side.y) + lag(cumsum(side.y), default = 0))/2  
  
  side.x <- switch(pos,
                   right = par("usr")[2]+(par("usr")[2]-par("usr")[1])*0.15,
                   left = par("usr")[1]-(par("usr")[2]-par("usr")[1])*0.15)
  side.x <- switch(typ,
                   un = rep(side.x, 4),
                   inter = rep(side.x, 5))  
  text(side.x, side.y, unique(s.df$hh.type))
  
  return(s.df)
}
