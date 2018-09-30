###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: aggregate inputs for analysis
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2,data.table, ggmap, usmap, mapproj)

# Set working directory
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out\\"
IN = "in\\"
##########################################################
## I. read in output data ################################
##########################################################

#calculate average solar profiles
list <- fread(paste0(DIR,IN,"optimization_list.csv"))
sol_agg <- data.frame()
sol_act <- data.frame()

for (i in 1:nrow(list)) {
  
  #create solar dataframe
  temp <- fread(paste0(DIR,IN,"sol_data\\", list[i,3],"_", list[i,4],".csv"))
  
  #convert local to time element
  temp$time <- as.POSIXct(strptime(temp$local_time_stor, "%Y-%m-%d %H:%M:%S"))
  temp$year <- as.numeric(format(temp$time, "%Y"))
  temp$month <- as.numeric(format(temp$time, "%m"))
  temp$day <- as.numeric(format(temp$time, "%d"))
  temp$hour <- format(temp$time, "%H")
  temp$id <- paste0(list[i,3],"_", list[i,4])
  
  temp.agg <- temp %>% group_by(hour, id) %>% summarize(avg = mean(generation)) %>% as.data.frame()
  
  sol_agg <- rbind(sol_agg,temp.agg)
  
  temp.act <- filter(temp, year == 2014, month == 10, day == 15)
  temp.act <- temp.act %>% group_by(hour, id) %>% summarize(avg = mean(generation)) %>% as.data.frame()
  
  sol_act <- rbind(sol_act,temp.act)
}

#calculate average load profiles
load_data = fread(paste0(DIR,IN,"\\id.csv"))
id <- id[which(load_data$ID!= 724365 & load_data$ID!= 724935 & load_data$ID!= 725477), ]

load_agg <- data.frame()
load_act <- data.frame()

for (i in 1:length(id$ID)) {
  
  #base case
  temp <- fread(paste0(DIR,IN,"\\res_load\\BASE\\",id[i], ".csv"), col.names=c("V","load"))
  temp$hr <- rep(1:24,365)
  temp$id <- id[i]
  temp$scen <- "BASE"
  
  temp.agg <- temp %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_agg <- rbind(load_agg,temp.agg)
  
  temp.act <- temp[6913:6937,]
  temp.act <- temp.act %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_act <- rbind(load_act,temp.act)
  
  #low case
  temp <- fread(paste0(DIR,IN,"\\res_load\\LOW\\",id[i], ".csv"), col.names=c("V","load"))
  temp$hr <- rep(1:24,365)
  temp$id <- id[i]
  temp$scen <- "LOW"
  
  temp.agg <- temp %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_agg <- rbind(load_agg,temp.agg)
  
  temp.act <- temp[6913:6937,]
  temp.act <- temp.act %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_act <- rbind(load_act,temp.act)
  
  #high case
  temp <- fread(paste0(DIR,IN,"\\res_load\\HIGH\\",id[i], ".csv"), col.names=c("V","load"))
  temp$hr <- rep(1:24,365)
  temp$id <- id[i]
  temp$scen <- "HIGH"
  
  temp.agg <- temp %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_agg <- rbind(load_agg,temp.agg)
  
  temp.act <- temp[6913:6937,]
  temp.act <- temp.act %>% group_by(hr, id, scen) %>% 
    summarize(avg = mean(load)*1000) %>% as.data.frame()
  
  load_act <- rbind(load_act,temp.act)
  
}

##output results
fwrite(sol_agg,paste0(DIR,OUT,"\\sol_agg.csv"))
fwrite(load_agg,paste0(DIR,OUT,"\\load_agg.csv"))
fwrite(sol_act,paste0(DIR,OUT,"\\sol_act.csv"))
fwrite(load_act,paste0(DIR,OUT,"\\load_act.csv"))

##########################################################
## II. plot load and solar ###############################
##########################################################
sol_agg <- fread(paste0(DIR,OUT,"\\sol_agg.csv"))
load_agg <- fread(paste0(DIR,OUT,"\\load_agg.csv"))
sol_act <- fread(paste0(DIR,OUT,"\\sol_act.csv"))
load_act <- fread(paste0(DIR,OUT,"\\load_act.csv"))

#solar plots
jpeg(filename = paste0(DIR,OUT,"\\images\\solar.jpg"), width = 950, height = 480)
ggplot(data=sol, aes(hr,out)) + geom_line(aes(color = id, group = id)) +
  xlab(label = "Hour of Day") + ylab(label = "Watt output (W)") +
  ggtitle(label = "Figure 2: Solar output multiplier across 933 locations")
dev.off()

#load plots
jpeg(filename = paste0(DIR,OUT,"\\images\\load.jpg"), width = 950, height = 480)
ggplot(data=load, aes(hr,avg)) + geom_line(aes(color = id, group = id)) +
  xlab(label = "Hour of Day") + ylab(label = "Watt consumption (W)") +
  ggtitle(label = "Figure 1: Load consumption across 933 locations")
dev.off()

##########################################################
## II. data analysis #####################################
##########################################################

ann <- load %>% group_by(id) %>% 
  summarize(kwh = sum(tot))

##########################################################
## II. geospatial plotting ###############################
##########################################################
##load in lat long for solar
opt_data <- fread(paste0(DIR,INPUT,"\\optimization_list.csv"))
sol_points <- opt_data[,c(6,5)]
load_points <- unique(opt_data[,c(12,11)])

# plotting the points where I have load and solar data
us <- map_data('world',
               c('usa', "hawaii", "alaska","puerto rico"))
ggplot()+
  geom_polygon(data=us, aes(x=long, y=lat, group = group), colour="grey20", fill="white")+
  geom_point(data = sol_points, aes(x = lon, y = lat.x, fill = "red"), 
             size = 3, shape = 21)+
  coord_map(projection = "mercator", xlim=c(-170, -60), ylim=c(15,65))+
  theme_bw()

ggplot()+
  geom_polygon(data=us, aes(x=long, y=lat, group = group), colour="grey20", fill="white")+
  geom_point(data = load_points, aes(x = long, y = lat.y, fill = "red"), 
             size = 3, shape = 21)+
  coord_map(projection = "mercator", xlim=c(-170, -60), ylim=c(15,65))+
  theme_bw()

### plotting the solar potential
plot_usmap(region = "county") + 
  geom_point(data = sol_points, aes(x = lon, y = lat.x, fill = "red", alpha = 0.8), 
             size = 2, shape = 21) +
  labs(title = "US Counties", subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"))
          
##########################################################
## II. TMY correlation analysis ##########################
##########################################################

# collect TMY months
list <- list.files(paste0(DIR,IN, "\\TMY"))
names <- as.numeric(str_extract(list, "\\-*\\d+\\.*\\d*"))
collect <- vector("list",length(list))

for (i in 1:length(list)) {
  
  #get tmy data
  tmy <- fread(paste0(DIR,IN,"\\TMY\\", list[i]),skip=1)
  tmy$month <- substr(tmy$`Date (MM/DD/YYYY)`,1,2)
  tmy$year <-  substr(tmy$`Date (MM/DD/YYYY)`, nchar(tmy$`Date (MM/DD/YYYY)`)-4+1, 
                      nchar(tmy$`Date (MM/DD/YYYY)`))
  
  #subset data of interest
  values <- unique(tmy[,c("year","month")])
  values$id <- names[i]
  
  #store values
  collect[[i]] <- values

}

tmy_dates <- do.call(rbind, collect)
saveRDS(tmy_dates,paste0(DIR,OUT, "\\tmy_dates"))

##aggregate solar/load data
list <- fread(paste0(DIR,IN,"\\optimization_list.csv"))
collect <- vector("list",length(list))

#aggregate solar/load data
for (i in 1:length(list)) {
  
  #get tmy data
  data <- data.frame(readRDS(paste0(DIR,IN,"\\all_data\\BASE_", list[i,3],"_",list[i,4])))

  #subset data of interest
  values <- tmy[,c("gen","load","month","year")]
  values$id <- list[i,10]
  
  #store values
  collect[[i]] <- values
  
}

#collect results
load_sol <- do.call(rbind, collect)

#merge on tmy_dates information
dates <- readRDS(paste0(DIR,OUT,"\\tmy_dates"))

load_sol <- merge(load_sol, dates, by = "id", all.x=TRUE)
                 