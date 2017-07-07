###############################################################################
# 1 FunSkippedGen
# 2 FunNumberGens
# 3 FunNewVars uses 1. and 2. to calculate new vars
# 4 FunSummarize uses full df from 4 and picks grouping variable. 
# 5 FunComplete takes all of the above and executes for eachcountry
# 6 FunPlot7
# 7 FunPlot4
###############################################################################


###############################################################################
# FunSkippedGen
# test for skipped generation: is there a missing number in the sequence of generations
###############################################################################

FunSkippedGen <- function(test){
  if(is.na(test[[1]])){return(0)} else {
    if(length(setdiff(seq(min(test, na.rm = TRUE), 
                          max(test, na.rm = TRUE)), 
                      test)) == 0) {
      return(0) } else {
        return(1)}
  }
}


###############################################################################
# FunNumberGens
# number of generations in HH, but test for institutions (first HH is NA)
###############################################################################

FunNumberGens <- function(test){
  if(is.na(test[1])){return(1)} else {
    return(length(unique(test[!is.na(test)])))
  }
}


###############################################################################
# FunNewVars
# open i-th country table and summarise hh type by gender/age group
# as well as the total national proprotions
###############################################################################

FunNewVars <- function(i){
  read.csv(paste0("data/", country.codes[i,2],".csv")) %>%   
    select(-COUNTRY, -YEAR, -SAMPLE ) %>% 
    left_join(rel.codes[,c(1,3)], by = c("RELATED" = "code")) %>%  # ADD GENERATION FROM LOOKUP TABLE
    mutate(RELATED = factor(RELATED,                           # ADD LABELS TO RELATED CODES FROM CODEBOOK 
                            levels = rel.codes$code, 
                            labels = rel.codes$label),
           RELATED = droplevels(RELATED),
           old = ifelse(AGE >= 60, 1, 0)) %>% 
    group_by(SERIAL) %>% 
    mutate(n.gen = as.numeric(FunNumberGens(gen)),
           hh.size = n(),
           gen.skipped = FunSkippedGen(gen),
           hh.type =  factor(ifelse(hh.size == 1, 1,
                                    ifelse(gen.skipped == 1, 2, n.gen+2 )), 
                             levels = 1:7, 
                             labels = c("single", "skipped", "one gen", 
                                        "two gen", "three gen", "four gen", "five gen")),
           hh.type4 = factor(ifelse(hh.size == 1, 1,
                                    ifelse(all(unique(RELATE) %in% c(1,2,3)), 2, 
                                           ifelse(5 %in% RELATE, 3, 4))), levels = 1:4,
                             labels = c("single", "nuclear",  "complex", "extended")))  %>% 
    ungroup() %>% 
    mutate(SEX = factor(SEX, labels = c("male", "female")),
           old = factor(old, labels = c("young", "old"))) 
}  


FunSummarize <- function(full.df, group.v) {
  full.df %>% 
    group_by(old, SEX, !!group.v) %>% 
    summarise( w.count = sum(PERWT))   %>% 
    unite(group, old,SEX) %>% 
    spread(key = group, value = w.count, fill = 0) %>% 
    mutate(total = rowSums(.[2:5])) %>% 
    gather(key=group, value = count, 2:6) %>% 
    group_by(group) %>% 
    mutate(prop = count/sum(count))
}

i = 19
FunComplete <- function(n) {
  for (i in 1:n){
  FunNewVars(i = i) -> full.df
  assign(paste0(country.codes[i,2], ".hh.gen"), 
         FunSummarize(full.df, dplyr::quo(hh.type)), pos = 1)
  assign(paste0(country.codes[i,2], ".hh.un"), 
         FunSummarize(full.df, dplyr::quo(hh.type4)), pos = 1)
  write.csv(eval(as.name(paste0(country.codes[i,2], ".hh.gen"))), 
            file = paste0("data/", country.codes[i,2],".hh.gen.csv"), row.names = FALSE)
  write.csv(eval(as.name(paste0(country.codes[i,2], ".hh.un"))), 
            file = paste0("data/", country.codes[i,2],".hh.un.csv"), row.names = FALSE)
  }
}


###############################################################################
# FunPlot7
# plot for intergenerational data
###############################################################################

FunPlot7 <- function(i) {
  hh.gen <- data.frame(code = c(1,2,3,4,4,4,5),
                       type = c("single",  "one gen", "two gen", "three gen",
                                "four gen", "five gen", "skipped"))

  df7 <- read.csv(paste0("data/", country.codes[i,2],".hh.gen.csv"))
  bars7 <- spread(select(df7, - count), group, prop)[1:4] %>% 
    left_join(hh.gen, c("hh.type" = "type")) %>% 
    arrange(code) %>% 
    group_by(code) %>% 
    summarise(old_male = sum(old_male),
              old_female = sum(old_female),
              total = sum(total))
  
  widths <- df7 %>%group_by(group) %>%  
    summarise(width = sum(count)) %>% 
    .[2:1, 1:2]
  background7 <- c(0, unlist(cumsum(bars7[4])))
  
  x<-barplot(as.matrix(bars7[2:3]),
             col = palette7, 
             width = pull(widths, 2),
             space = 1.3, 
             axes = FALSE,
             names.arg = c(NA,NA))
  text(x, y=-0.07, c("A", "B"))
  rect(rep(par("usr")[1]*1.2, 5), background7[1:5], 
       rep(par("usr")[2]*0.9, 5), 
       background7[2:6],
       border = "white", lwd = 1,
       col =    palette7)
  
  rect(rep(par("usr")[1]*1.2, 5), background7[1:5], 
       rep(par("usr")[2]*0.9, 5), 
       background7[2:6],
       col = "white", density = 20, angle = 30, lwd = 3, border =NA)
  
  barplot(as.matrix(bars7[2:3]),
          col = palette7, 
          width = pull(widths, 2),
          space = 1.3, add = TRUE,
          axes = FALSE,
          names.arg = c(NA,NA))
  axis(2, labels = rep(NA,6))
  midpoints7 <- (cumsum(pull(bars7,3)) - lag(cumsum(pull(bars7,3)), default = 0))/2 + 
    lag(cumsum(pull(bars7,3)), default = 0)
  
  text(rep(par("usr")[2], 5), midpoints7, letters[1:5])
}

###############################################################################
# FunPlot4
# plot for family data
###############################################################################

FunPlot4 <- function(i) {
  hh.un <- data.frame(code = c(1:4),
                      type = c("single",  "nuclear", 
                               "extended", "complex"))
  df4 <- read.csv(paste0("data/", country.codes[i,2],".hh.un.csv"))
  bars4 <- spread(select(df4, - count), group, prop)[1:4] %>% 
    left_join(hh.un, c("hh.type4" = "type")) %>% 
    arrange(code) 
  widths <- df4 %>%group_by(group) %>%  
    summarise(width = sum(count)) %>% 
    .[2:1, 1:2]
  background4 <- c(0, unlist(cumsum(bars4[4])))
  
  x <- barplot(as.matrix(bars4[3:2]),
          col = palette4, 
          width = pull(widths, 2),
          space = 1.3,
          axes = FALSE,
          names.arg = c(NA,NA))
  text(x, y=-0.07, c("A", "B"))
  
  rect(rep(par("usr")[1]*1.2, 4), background4[1:4], rep(par("usr")[2]*0.9, 4), 
       background4[2:5],
       border = "white", lwd = 1,
       col =    palette4)
  
  rect(rep(par("usr")[1]*1.2, 4), background4[1:4], rep(par("usr")[2]*0.9, 4), 
       background4[2:5],
       col = "white", density = 20, angle = 30, lwd = 3, border =NA)
  
  barplot(as.matrix(bars4[3:2]),
          col = palette4, 
          width = pull(widths, 2),
          space = 1.3, add = TRUE,
          axes = FALSE,
          names.arg = c(NA,NA))
  axis(4, labels = c(5,6,7,8,9,0), at = seq(0,1,0.2), las = 2, hadj = -1.4)
  midpoints4 <- (cumsum(pull(bars4,3)) - lag(cumsum(pull(bars4,3)), default = 0))/2 + 
    lag(cumsum(pull(bars4,3)), default = 0)
  
  text(rep(par("usr")[1], 6), midpoints4, bars4[[5]])
}

