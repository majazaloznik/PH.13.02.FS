## only run one time
###############################################################################
## 00. preliminaries  #########################################################
## 01. setting up import   ####################################################
## 02. importing data and splitting into country csv.s ########################
## 03. summarise individual countires #########################################
## 04.combine all summaries in one table ######################################
###############################################################################
## OUTPUT: "data/summaries.csv" - 985 rows x 10 varz - all you need for plots.
###############################################################################

## 00. preliminaries  #########################################################
library(sqldf)
library(dplyr)
library(tidyr)
source("scripts/02-myfunctions.R")


## 01. setting up import   ####################################################
# How was the data gathered:
#'
#' Only some IPUMS collecctions have the detailed codes for RELATE
#' in particular we want grandchildren and gradparents, otherwise we can't get
#' any reasonable generation numbers. 
#' So I went manually through https://international.ipums.org/international-action/variables/RELATE#codes_section
#' to see which ones do, and selected only the ones with a census from this millenium.
#' repeated this 16.6, becasue new countries were added - Kenya and Portugal
#' manual selection: 
#' Person vars ' RELATE, AGE AND SEX
#' 22 SAMPLES
#' that ends up being 10 variables 
# years <- c(2011, #AM = Armenia
#            2009, # BY Belarus
#            2011, # BW - Botswana
#            2010, # BR - Brazil
#            2000, # Cn China
#            2010, # Dominican
#            2001, # Greece
#            2011, # irenald
#            2009, # KE Kenya **
#            2009, # Kyrgyz
#            2015, # Mexico
#            2000, # Mongolia
#            2005, # Nicaragua
#            2007, # Palestine
#            2010, # Panama
#            2002, # Poland
#            2011, # Portugal **
#            2000, # Peurto rico
#            2002, # Romania
#            2002, # Senegal
#            2011, # South Africa
#            2000 # United states
# )
# 
# subregions <- c("WAS",
#                 "EEU",
#                 "SAF",
#                 "SAM",
#                 "EAS",
#                 "CRB",
#                 "SEU",
#                 "NEU",
#                 "EAF",
#                 "CAS",
#                 "CAM",
#                 "EAS",
#                 "CAM",
#                 "WAS",
#                 "CAM",
#                 "EEU",
#                 "SEU",
#                 "CRB",
#                 "EEU",
#                 "WAF",
#                 "SAF",
#                 "NAM")
# 
# regions <- c("AS",
#              "EU",
#              "AF",
#              "AM",
#              "AS",
#              "AM",
#              "EU",
#              "EU",
#              "AF",
#              "AS",
#              "AM",
#              "AS",
#              "AM",
#              "AS",
#              "AM",
#              "EU",
#              "EU",
#              "AM",
#              "EU",
#              "AF",
#              "AF",
#              "AM"
#              )
# 
# # from the ipums codebook impums_00006.cbk, manually copy/pasted the
# # codes for RELATED and in the spreadsheet manually added generation signifiers
# # import lookup tables for relationships and country codes (these were generated manually from the codebook)
# rel.codes <- read.csv("data/relationship-codes.csv")
# country.codes <- read.csv("data/countrycodes.csv")
# country.codes$country <- as.character(country.codes$country)
# country.codes$year <- years
# country.codes$subregions <- subregions
# country.codes$regions <- regions
# rm(years, subregions, regions)
# 
# 
# write.csv(country.codes, "data/country.codes.csv", row.names = FALSE)


## 02. importing data and splitting into country csv.s ########################
# # split file into 20 (actually had to do this in two sets, because of some
# # sql disk full error, but same thing
# for (i in (1:22)[-c(9,17)]){
#   assign(country.codes[i,2], read.csv.sql("data-raw/ipumsi_00006.csv", 
#                                           sql = paste("select * from file where COUNTRY =", country.codes[i,1] ), eol = "\n"))
#   write.csv(eval(as.name(country.codes[i,2])), file = paste0("data/", country.codes[i,2],".csv"), row.names = FALSE)
#   rm(list = country.codes[i,2])
#   }
# 
# # this are the two countries added later, this code was retro-written
# for (i in c(9, 17)){
#   assign(country.codes[i,2], read.csv.sql("data-raw/ipumsi_00007.csv", 
#                                           sql = paste("select * from file where COUNTRY =", country.codes[i,1] ), eol = "\n"))
#   write.csv(eval(as.name(country.codes[i,2])), file = paste0("data/", country.codes[i,2],".csv"), row.names = FALSE)
#   rm(list = country.codes[i,2])
# }


## 03. summarise individual countires #########################################
# add generation number from lookup table, as well as labels for relationships
# then binary for old/young
# then group into HH and calculate n. of generations for
# hh. size and two different types of households. 
# finally summarise proportions for each of the types of HHs
# do this one country at a time,because the large df is really large

# now save all 44 tables 
# FunComplete(nrow(country.codes))


## 04.combine all summaries in one table ######################################

# df <- data.frame(NULL)
# for(i in 1:nrow(country.codes)) {
#   x.gen <- read.csv(paste0("data/", country.codes[i,2],".hh.gen.csv"))
#   # also collapse the 4 and 5 generation households here:
#   hh.7gen <- data.frame(code = c(1,2,3,4,4,4,5),
#                         hh.type = c("single",  "one gen", "two gen", "three gen",
#                                     "four gen", "five gen", "skipped"))
#   hh.5gen <- data.frame(code = c(1,2,3,4,5),
#                         hh.type = factor(1:5, labels = c("single",  "one gen", "two gen", "three+ gen",
#                                                          "skipped")))
#   x.gen %>% 
#     left_join(hh.7gen) %>% 
#     arrange(code) %>% 
#     group_by(code, group) %>% 
#     summarise_if(is.numeric, sum) %>% 
#     left_join(hh.5gen) %>%
#     ungroup() %>% 
#     select(-code) %>% 
#     arrange(group, hh.type) -> x.gen
#   
#   x.un <- read.csv(paste0("data/", country.codes[i,2],".hh.un.csv"))  
#   x.un <- rename(x.un, hh.type = hh.type4)
#   x.gen <- cbind(x.gen, typology = "inter")
#   x.un <- cbind(x.un, typology = "un")
#   x <- rbind(x.gen, x.un)
#   x$hh.type <- factor(x$hh.type,levels(x$hh.type)[c(1:5,8,7,6)])
#   x <- cbind(x, country.codes[i,])
#   df <- rbind(df, x)
# }
# 
# write.csv(df, file = "data/summaries.csv", row.names = FALSE)
