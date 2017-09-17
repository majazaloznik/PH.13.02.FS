# Household Living Arrangements of Elderly People

## Proportions of over 60s by household type and intergenerational structure in selected countries

This is the repository for the data gathering, cleaning and plotting as well as the graphic design for the Population Horizon Factsheet IV (2016, vol 13, issue 2). 

This README includes an overview of the:

1. Brief
2. Data import
3. Outputs:
  * Summary tables
  * Factsheet [pdf](poster/PH.13.02.FS.pdf)
  * Interactive application (TODO)
 
## Brief

The prevalance of different household living arrangements varies across countries and across different population groups. This factsheet focuses on people over 60 years of age and compares men and women in each country. 

Using census microdata collected in the IPUMS-International database we summarise the types of households in which the elderly live using  two typologies:

* http://www.ntvspor.net/canli-yayinthe household composition based on family nuclei, and
* the intergenerational structure of the household. 

## Data Import

The data import and tidying process is not fully automated since the raw data needed to be manually extracted from [IPUMS](https://international.ipums.org/international/)

### Raw Data

Data selection: I picked (manually) all country samples from IPUMS that (according to this [table](https://international.ipums.org/international-action/variables/RELATE#codes_section)):

* have RELATED (RELATE detailed) codes for Grandchildren and Grandparents 
* have a sample in the 2000s.

This leaves us with 22 countries and samples, I've added the UN regional and subregional classifications:

| id| code|country           | year|subregions |regions |
|--:|----:|:-----------------|----:|:----------|:-------|
|  1|   51|Armenia           | 2011|WAS        |AS      |
|  2|  112|Belarus           | 2009|EEU        |EU      |
|  3|   72|Botswana          | 2011|SAF        |AF      |
|  4|   76|Brazil            | 2010|SAM        |AM      |
|  5|  156|China             | 2000|EAS        |AS      |
|  6|  214|DominicanRepublic | 2010|CRB        |AM      |
|  7|  300|Greece            | 2001|SEU        |EU      |
|  8|  372|Ireland           | 2011|NEU        |EU      |
|  9|  404|Kenya             | 2009|EAF        |AF      |
| 10|  417|KyrgyzRepublic    | 2009|CAS        |AS      |
| 11|  484|Mexico            | 2015|CAM        |AM      |
| 12|  496|Mongolia          | 2000|EAS        |AS      |
| 13|  558|Nicaragua         | 2005|CAM        |AM      |
| 14|  275|Palestine         | 2007|WAS        |AS      |
| 15|  591|Panama            | 2010|CAM        |AM      |
| 16|  616|Poland            | 2002|EEU        |EU      |
| 17|  620|Portugal          | 2011|SEU        |EU      |
| 18|  630|PuertoRico        | 2000|CRB        |AM      |
| 19|  642|Romania           | 2002|EEU        |EU      |
| 20|  686|Senegal           | 2002|WAF        |AF      |
| 21|  710|SouthAfrica       | 2011|SAF        |AF      |
| 22|  840|UnitedStates      | 2000|NAM        |AM      |

### Data Extract

The data extract is downloaded as a single file - This dataset is over 3GB and therefore too large to import into R, so an sql query is used to extract individual country samples. Neither of these two sets of files are available here for licencing reasons, but the relevant summaries are. 

### Data Summaries

New variables

Definitions



## Outputs

### Summary Tables 

### Factsheet

### Shiny application TODO