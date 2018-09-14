###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: get location IDs; also match IDs of solar with load 
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, rjson, maps,maptools, spatstat,rgeos, broom, data.table)

# Set working directory
DIR <- "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT <- "in\\res_solar"
IN <-  "in\\"
LD<- "LOW"

##########################################################
## I. Get lists of file names ############################
##########################################################

solar <- list.files(paste0(DIR,OUT))
solar_clean <- as.numeric(substr(solar, 1,6))

load <- list.files(paste0(DIR, IN,"res_load\\",LD))
load_clean <- as.numeric(str_extract(load, "\\-*\\d+\\.*\\d*"))
df <- data.frame(x = load, y= load_clean)

id <- intersect(solar_clean,load_clean)
write.csv(id,paste0(DIR, IN,"id.csv"))

#re-name files
subdir <- paste0(DIR,IN,"\\res_load\\",LD)
file.rename(from = file.path(subdir, load), to = file.path(subdir, paste0(load_clean,".csv")))

## clean load files
load_list <- list.files(paste0(DIR,IN,"\\res_load\\",LD))

for (i in 1:length(load_list)) {
  
  temp <- read.csv(paste0(DIR,IN,"res_load\\",LD,"\\", load_list[i]))
  temp_t <- as.data.frame(as.numeric(temp[,2]))
  write.csv(temp_t, paste0(DIR, IN,"res_load\\",LD,"\\", load_list[i]))
}


##########################################################
## II Match solar and load data ##########################
##########################################################
##load in lat long for solar
sol_data <- fread(paste0(DIR,IN,"solar_list.csv"))
cord1 <- sol_data[,c(4,3)]

##load in lat long for tmy3
tmy_data <- fromJSON(file = paste0(DIR,IN,"\\tmy3_lat_lng.json"))
names <- data.frame(names(tmy_data))
tmy_coor <- data.frame(matrix(unlist(tmy_data), nrow = 1020, byrow=T))
tmy_coor <-cbind(names, tmy_coor)
colnames(tmy_coor) <- c("ID", "lat","long")
tmy_coor$ID <- as.numeric(levels(tmy_coor$ID))[tmy_coor$ID]

##get IDs of actual load data
load_data <- fread(paste0(DIR,IN,"id.csv"))
load_data <- merge(load_data, tmy_coor, by="ID")
load_data$row_in_2 <- as.numeric(row.names(load_data))

#pull out the coords
cord2 <- load_data[,c(3,2)]

#find nearest load to solar data
set1sp <- SpatialPoints(cord1)
set2sp <- SpatialPoints(cord2)
sol_data$row_in_2 <- apply(gDistance(set2sp, set1sp, byid=TRUE), 1, which.min)

##connect to the ID
final <- merge(sol_data, load_data, by="row_in_2")

#output
write.csv(final,paste0(DIR, IN,"optimization_list.csv"))


