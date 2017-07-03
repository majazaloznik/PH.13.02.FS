


###############################################################################
# 1 FunSkippedGen
# 2 FunNumberGens
# 3 FunSummarize (uses 1. and 2. )
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
# FunSummarize
# open i-th country table and summarise hh type by gender/age group
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
    summarise(count = n(), w.count = sum(PERWT))  %>% 
    mutate(prop = count/sum(count), w.prop = w.count/sum(w.count))   
}

