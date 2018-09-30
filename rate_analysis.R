###############################################################################
###
###   Rate Analysis
###   PURPOSE: calcualte various rate designs
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2,sp, foreign)

# Set working directory
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out"
IN = "in"

##########################################################
## I. read in rate data ##################################
##########################################################
#collect geographic information
county_zip = readRDS(paste0(DIR,IN,"\\Stephen\\final\\countytozip.rds"))

#collect geographic names
county_names <- read.csv(paste0(DIR,IN,"\\Stephen\\county_names.csv"))
county_names$code <- as.character(county_names$code)
county_names$state_fips <- as.numeric(ifelse(nchar(county_names$code) == 4, substring(county_names$code,1,1),
                                  substring(county_names$code,1,2)))

county_names$county_fips <- as.numeric(ifelse(nchar(county_names$code) == 4, substring(county_names$code,2,4),
                                             substring(county_names$code,3,5)))

county_names$county <- tolower(county_names$county)
county_names$state <- tolower(county_names$state)

county_zip <- merge(county_zip, county_names[,c(2:5)], by = c("state_fips","county_fips"))

#tmypoints = readRDS(paste0(DIR,IN,"\\Stephen\\final\\pointsTMY3.rds"))
utilitytozip = readRDS(paste0(DIR,IN,"\\Stephen\\final\\utilitytozip.rds"))
colnames(utilitytozip)[1] <- "eia_id_e"
utilitytozip <- filter(utilitytozip, year == 2016)

#combine county to zip
utilitytozip_comb <- merge(utilitytozip[,c(1,3)], county_zip[,c(3:5)], by = "zip")


##########################################################
## II. calculate rates ##################################
##########################################################
#collect rate information
rates = read.dta(paste0(DIR,IN,"\\Stephen\\final\\output.dta"))

#only pick 2016
final_rates <- filter(rates, year == 2016)

#pick columns of interest
keeps <- c("state","eia_id_e","res_cust","res_sales","res_rev","fixedcharge","varcharge")
final_rates <- final_rates[keeps]

#calc rate designs of interest
final_rates$curr_ratio <- final_rates$fixedcharge * 12 * final_rates$res_cust / final_rates$res_rev

final_rates$r_curr_fixed <- final_rates$fixedcharge * 12
final_rates$r_curr_variable <- final_rates$varcharge

final_rates$r_0_fixed <- 0
final_rates$r_0_variable <- final_rates$res_rev/final_rates$res_sales * 100

final_rates$r_0.25_fixed <- final_rates$res_rev * 0.25 / final_rates$res_cust
final_rates$r_0.25_variable <- final_rates$res_rev * 0.75 / final_rates$res_sales * 100

final_rates$r_0.5_fixed <- final_rates$res_rev * 0.5 / final_rates$res_cust
final_rates$r_0.5_variable <- final_rates$res_rev * 0.5 / final_rates$res_sales * 100

final_rates$r_0.75_fixed <- final_rates$res_rev * 0.75 / final_rates$res_cust
final_rates$r_0.75_variable <- final_rates$res_rev * 0.25 / final_rates$res_sales * 100
  
final_rates$r_1_fixed <- final_rates$res_rev/final_rates$res_cust
final_rates$r_1_variable <- 0

##########################################################
## III. combine geography #################################
##########################################################

county_rates <- merge(utilitytozip_comb[,c(2,3,4)], final_rates[,c(1,2,8:20)], by = "eia_id_e")

county_rates <- county_rates[!duplicated(county_rates[,c('eia_id_e', 'county', 'state.x')]),]
county_rates <- county_rates[,c(2,3,5:17)]
out_rates <- county_rates %>% group_by(county, state.x) %>% summarise_all(funs(median))






