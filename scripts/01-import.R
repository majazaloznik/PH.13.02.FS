###############################################################################


library(sqldf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
source("scripts/02-myfunctions.R")

###############################################################################
###############################################################################
## How was the data gathered:
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
years <- c(2011, #AM = Armenia
           2009, # BY Belarus
           2011, # BW - Botswana
           2010, # BR - Brazil
           2000, # Cn China
           2010, # Dominican
           2001, # Greece
           2011, # irenald
           2009, # KE Kenya **
           2009, # Kyrgyz
           2015, # Mexico
           2000, # Mongolia
           2005, # Nicaragua
           2007, # Palestine
           2010, # Panama
           2002, # Poland
           2011, # Portugal **
           2000, # Peurto rico
           2002, # Romania
           2002, # Senegal
           2011, # South Africa
           2000 # United states
)

subregions <- c("WAS",
                "EEU",
                "SAF",
                "SAM",
                "EAS",
                "CRB",
                "SEU",
                "NEU",
                "EAF",
                "CAS",
                "CAM",
                "EAS",
                "CAM",
                "WAS",
                "CAM",
                "EEU",
                "SEU",
                "CRB",
                "EEU",
                "WAF",
                "SAF",
                "NAM")

regions <- c("AS", 
             "EU",
             "AF",
             "AM",
             "AS",
             "AM",
             "EU",
             "EU",
             "AF",
             "AS",
             "AM",
             "AS",
             "AM",
             "AS",
             "AM",
             "EU",
             "EU",
             "AM",
             "EU",
             "AF",
             "AF",
             "AM"
             )
###############################################################################
# IMPORT - setup
###############################################################################
# from the ipoms codebook impums_00006.cbk, manually copy/pasted the 
# codes for RELATED and in the spreadsheet manually added generation signifiers
# import lookup tables for relationships and country codes (these were generated manually from the codebook)
rel.codes <- read.csv("data/relationship-codes.csv")
country.codes <- read.csv("data/countrycodes.csv")
country.codes$country <- as.character(country.codes$country)
country.codes$year <- years
country.codes$subregions <- subregions
country.codes$regions <- regions
rm(years, subregions, regions)



country.codes %>%  arrange(regions, subregions)
# IMPORT - ONE OFF
###############################################################################
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


# IMPORT - ONE OFF
###############################################################################
## summaries for individual countires
###############################################################################
# add generation number from lookup table, as well as labels for relationships
# then binary for old/young

# group into HH and calculate n. of generations
# hh. size

FunNewVars(i = 18) -> full.df
table(full.df$RELATE)
FunSummarize(full.df, dplyr::quo(hh.type))
FunSummarize(full.df, dplyr::quo(hh.type4))

x.sum1  %>% 
  unite(category, old, SEX) %>% 
    select(-count) %>% 
    spread(category, prop) -> x.heights

x.sum1  %>% 
  unite(category, old, SEX) %>% 
  select(-prop) %>% 
  group_by(category) %>% 
  summarise(sum = sum(count))-> x.widths

barplot(as.matrix(x.heights[,2:5]) , 
        width = x.widths[[2]], 
        legend.text = x.heights[[1]],
        col = brewer.pal(7, "Dark2"))
