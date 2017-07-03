###############################################################################
# 1 FunSkippedGen
# 2 FunNumberGens
# 3 FunNewVars uses 1. and 2. to calculate new vars
# 4 FunSummarize uses full df from 4 and picks grouping variable. 
# 5 FunComplete takes all of the above and executes for eachcountry
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
    spread(key = group, value = w.count) %>% 
    mutate(total = rowSums(.[2:5])) %>% 
    gather(key=group, value = count, 2:6) %>% 
    group_by(group) %>% 
    mutate(prop = count/sum(count))
}



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
