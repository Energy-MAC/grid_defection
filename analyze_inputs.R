###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: aggregate inputs for plotting
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2,data.table, ggmap, usmap, mapproj)

# Set working directory
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out"
INPUT = "in"
##########################################################
## I. read in output data ################################
##########################################################
id = fread(paste0(DIR,INPUT,"\\id.csv"))

sol <- data.frame()
for (i in 1:length(id$ID)) {
  temp.full <- fread(paste0(DIR,INPUT,"\\res_solar\\",id[i], "TYA.CSV.csv"))
  temp.full$hr <- rep(1:24,365)
  temp.full$id <- id[i]
  
  temp <- temp.full %>% group_by(hr, id) %>% summarize(out = mean(Watt)) %>% as.data.frame()
  
  
  sol <- rbind(sol,temp)
}


load <- data.frame()
for (i in 1:length(id$ID)) {
  temp.full <- fread(paste0(DIR,INPUT,"\\res_load\\BASE\\",id[i], ".csv"))
  colnames(temp.full) <- c("num","Watt")
  temp.full$hr <- rep(1:24,365)
  temp.full$id <- id[i]
  
  temp <- temp.full %>% group_by(hr, id) %>% 
    summarize(avg = mean(Watt)*1000,
               tot = sum(Watt)) %>% as.data.frame()
  
  
  load <- rbind(load,temp)
}

##output results
fwrite(sol,paste0(DIR,OUT,"\\sol_agg.csv"))
fwrite(load,paste0(DIR,OUT,"\\load_agg.csv"))

##########################################################
## II. plot load and solar ###############################
##########################################################
sol <- fread(paste0(DIR,OUT,"\\sol_agg.csv"))
load <- fread(paste0(DIR,OUT,"\\load_agg.csv"))

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
          
