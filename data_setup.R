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
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
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
load_data <- load_data[which(load_data$ID!= 724365 & load_data$ID!= 724935 & load_data$ID!= 725477), ]
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


##########################################################
## III. Combining input data #############################
##########################################################
list <- fread(paste0(DIR,IN,"optimization_list.csv"))

for (i in 1:nrow(list)) {
  
  #create load dataframes
  temp_low <- fread(paste0(DIR,IN,"res_load\\LOW\\", list[i,10],".csv"), col.names=c("V","load"))
  temp_low <- do.call("rbind", replicate(9,temp_low, simplify = FALSE))
  temp_med <- fread(paste0(DIR,IN,"res_load\\BASE\\", list[i,10],".csv"), col.names=c("V","load"))
  temp_med <- do.call("rbind", replicate(9,temp_med, simplify = FALSE))
  temp_high <- fread(paste0(DIR,IN,"res_load\\HIGH\\", list[i,10],".csv"), col.names=c("V","load"))
  temp_high <- do.call("rbind", replicate(9,temp_high, simplify = FALSE))
  
  #create solar dataframe
  sol <- fread(paste0(DIR,IN,"sol_data\\", list[i,3],"_", list[i,4],".csv"))
  
  #convert local to time element
  sol$time <- as.POSIXct(strptime(sol$local_time_stor, "%Y-%m-%d %H:%M:%S"))
  sol$year <- format(sol$time, "%Y")
  sol$month <- format(sol$time, "%m")
  sol$day <- format(sol$time, "%d")
  sol$hour <- format(sol$time, "%H")
  sol <- sol[,c(5,6,7,8,3)]
  sol <- sol[complete.cases(sol), ]
  
  #create appended table
  year <- c("2008","2009","2010","2011","2012","2013","2014","2015","2016")
  month <- c("03","03","03","03","03","03","03","03","03")
  day <- c("09","08","14","13","11","10","09","08","13")
  hour <- c("02","02","02","02","02","02","02","02","02")
  generation <- c(0,0,0,0,0,0,0,0,0)
  add <- data.frame(year,month,day,hour,generation)
  sol<- rbind(sol,add)
  
  #drop feb 29 data (maybe by dropping duplicates)
  sol1 <- sol %>% group_by(year,month, day, hour) %>% summarise(gen = mean(generation))
  sol1 <- data.frame(lapply(sol1, function(x) as.numeric(as.character(x))))
 
  #merge columns and write table
  dt <- cbind(sol1[,c(1,2,3,5)], temp_med[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data\\BASE_", list[i,3],"_", list[i,4]))
  
  dt <- cbind(sol1[,c(1,2,3,5)], temp_high[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data\\HIGH_", list[i,3],"_", list[i,4]))
  
  dt <- cbind(sol1[,c(1,2,3,5)], temp_low[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data\\LOW_", list[i,3],"_", list[i,4]))
}

## save rds files as csvs
DIR <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
IN <-  "in\\"

list <- list.files(paste0(DIR,IN,"all_data\\R files"))

for (i in 1:length(list)) {
  
  out <- readRDS(paste0(DIR, IN,"all_data\\R files\\",list[i]))
  
  write.csv(out,paste0(DIR, IN,"all_data\\",list[i],".csv"))
  
}
##########################################################
## III. adding load data to optimization file ############
##########################################################

list <- fread(paste0(DIR,IN,"optimization_list.csv"))

low <- c()
med <- c()
high <- c()

for (i in 1:nrow(list)) {
  
  #create load dataframes
  temp_low <- fread(paste0(DIR,IN,"res_load\\LOW\\", list[i,10],".csv"), col.names=c("V","load"))
  low <- append(low, sum(temp_low$load))
  temp_med <- fread(paste0(DIR,IN,"res_load\\BASE\\", list[i,10],".csv"), col.names=c("V","load"))
  med <- append(med, sum(temp_med$load))
  temp_high <- fread(paste0(DIR,IN,"res_load\\HIGH\\", list[i,10],".csv"), col.names=c("V","load"))
  high <- append(high, sum(temp_high$load))

}

list$low_energy <- low
list$med_energy <- med
list$high_energy <- high

write.csv(list,paste0(DIR, IN,"optimization_list_energy.csv"))

##########################################################
## IV. Compile results file ##############################
##########################################################



list <- list.files(paste0(DIR,OUT,"500pv_100stor\\"))
results <- data.frame()

for (i in 1:length(list)) {
  
  out <- read.csv(paste0(DIR, IN,"500pv_100stor\\",list[i]))
  out$case <- gsub(".*[_]([^.]+)[.].*", "\\1", list[i])
  results <- rbind(results,out)
}

write.csv(out,paste0(DIR, IN,"all_data\\",list[i],".csv"))