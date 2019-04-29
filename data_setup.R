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
p_load(magrittr, dplyr, stringr, rjson, maps,maptools, spatstat,rgeos, 
       broom, data.table, tidyr, lubridate)

# Set working directory
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
#DIR <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
OUT <- "out\\"
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
  temp_low <- do.call("rbind", replicate(8,temp_low, simplify = FALSE))
  temp_med <- fread(paste0(DIR,IN,"res_load\\BASE\\", list[i,10],".csv"), col.names=c("V","load"))
  temp_med <- do.call("rbind", replicate(8,temp_med, simplify = FALSE))
  temp_high <- fread(paste0(DIR,IN,"res_load\\HIGH\\", list[i,10],".csv"), col.names=c("V","load"))
  temp_high <- do.call("rbind", replicate(8,temp_high, simplify = FALSE))
  
  #create solar dataframe
  DIR2 <- "G:\\Team Drives\\Renewable_Profile\\Analysis\\all_dpv\\"
  sol <- fread(paste0(DIR2,OUT,"solar_profile (1998-2005)\\", list[i,3],"_", list[i,4],".csv"))
  
  #convert local to time element
  sol$time <- ymd_hms(sol$local_time_stor)
  sol$year <- format(sol$time, "%Y")
  sol$month <- format(sol$time, "%m")
  sol$day <- format(sol$time, "%d")
  sol$hour <- format(sol$time, "%H")
  sol <- sol[,c(5,6,7,8,3)]
  
  #drop feb 29 data (maybe by dropping duplicates b/c they were all marked as feb 28th data)
  sol1 <- sol %>% group_by(year,month, day, hour) %>% summarise(gen = mean(generation))
  sol1 <- data.frame(lapply(sol1, function(x) as.numeric(as.character(x))))
 
  #merge columns and write table
  dt <- cbind(sol1[,c(1,2,3,5)], temp_med[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data_1998-2005\\BASE_", list[i,3],"_", list[i,4]))
  
  dt <- cbind(sol1[,c(1,2,3,5)], temp_high[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data_1998-2005\\HIGH_", list[i,3],"_", list[i,4]))
  
  dt <- cbind(sol1[,c(1,2,3,5)], temp_low[,2])
  saveRDS(dt, paste0(DIR, IN,"all_data_1998-2005\\LOW_", list[i,3],"_", list[i,4]))
}

## save rds files as csvs
DIR2 <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
#DIR2 <- "C:\\Users\\Will\\Desktop\\data\\"
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

low_max <- c()
med_max <- c()
high_max <- c()

low_min <- c()
med_min <- c()
high_min <- c()

for (i in 1:nrow(list)) {
  
  #create load dataframes
  temp_low <- fread(paste0(DIR,IN,"res_load\\LOW\\", list[i,10],".csv"), col.names=c("V","load"))
  low <- append(low, sum(temp_low$load))
  low_max <- append(low_max, max(temp_low$load))
  low_min <- append(low_min, min(temp_low$load))
  temp_med <- fread(paste0(DIR,IN,"res_load\\BASE\\", list[i,10],".csv"), col.names=c("V","load"))
  med <- append(med, sum(temp_med$load))
  med_max <- append(med_max, max(temp_med$load))
  med_min <- append(med_min, min(temp_med$load))
  temp_high <- fread(paste0(DIR,IN,"res_load\\HIGH\\", list[i,10],".csv"), col.names=c("V","load"))
  high <- append(high, sum(temp_high$load))
  high_max <- append(high_max, max(temp_high$load))
  high_min <- append(high_min, min(temp_high$load))

}

list$low_energy <- low
list$med_energy <- med
list$high_energy <- high

list$low_max <- low_max
list$med_max <- med_max
list$high_max <- high_max

list$low_min <- low_min
list$med_min <- med_min
list$high_min <- high_min

write.csv(list,paste0(DIR, IN,"optimization_list_energy.csv"))

##########################################################
## IV. Aggregate solar data ##############################
##########################################################
DIR2 <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
DIR3 <- "G:\\Team Drives\\Renewable_Profile\\Analysis\\all_dpv\\out\\solar_profile (2008-2016)\\"

list <- fread(paste0(DIR2,IN,"optimization_list.csv"))

sol_collection <- data.frame()

for (i in 1:nrow(list)) {
  
  #load solar data
  sol <- fread(paste0(DIR3, list[i,3],"_", list[i,4],".csv"))
  
  #create output
  sol_summ <- sol %>% group_by() %>% 
    summarize(len = length(generation),gen=sum(generation))
  
  sol_summ$county <-as.character(list[i,3])
  sol_summ$state <- as.character(list[i,4])
  
  sol_collection <- rbind(sol_collection,as.data.frame(sol_summ))

}

write.csv(sol_collection,paste0(DIR2, OUT,"solar_collection.csv"))

##########################################################
## IV. Compile results file ##############################
##########################################################
DIR2 <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
#DIR2 <- "C:\\Users\\Will\\Desktop\\data\\Analysis\\"

folder <- "600pv_100stor (min const @4% DR)"

list <- list.files(paste0(DIR2,OUT,folder,"\\"))
list <- list[lapply(list,function(x) length(grep("outcome",x,value=FALSE))) == 0]
results <- data.frame()

for (i in 1:length(list)) {
  
  out <- read.csv(paste0(DIR2, OUT,folder,"\\",list[i]))
  out$id <- substr(list[i],1,nchar(list[i]) - 4)
  out <- out %>% separate(id, c("remove","reliability","case","county","state"), "_")
  out$remove <- NULL
  results <- rbind(results,out)
}

write.csv(results,paste0(DIR2, OUT,folder,".csv"))

test <- results %>% group_by(county,state) %>% 
  summarize(len = length(pv))

write.csv(test,paste0(DIR2, OUT,"missings.csv"))
