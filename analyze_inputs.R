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
p_load(magrittr, dplyr, stringr, ggplot2,data.table, ggmap, usmap, mapproj,plyr)

# Set working directory
DIR <- "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
#DIR <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
OUT = "out\\"
IN = "in\\"
##########################################################
## I. analyze solar and load #############################
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
id <- load_data[which(load_data$ID!= 724365 & load_data$ID!= 724935 & load_data$ID!= 725477), ]

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
theme_plot <- theme(
  legend.position = "right",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.spacing.x = unit(0.3, "cm"))

sol_agg <- fread(paste0(DIR,OUT,"\\sol_agg.csv"))
load_agg <- fread(paste0(DIR,OUT,"\\load_agg.csv"))
sol_act <- fread(paste0(DIR,OUT,"\\sol_act.csv"))
load_act <- fread(paste0(DIR,OUT,"\\load_act.csv"))

sol_max <- sol_agg %>% group_by(id) %>% summarize(max = max(avg))
sol_agg <- merge(sol_agg, sol_max, by="id")

sol_max <- sol_act %>% group_by(id) %>% summarize(max = max(avg))
sol_act <- merge(sol_act, sol_max, by="id")

#solar plots
jpeg(filename = paste0(DIR,OUT,"\\images\\solar_avg.jpg"), width = 500, height = 480)
ggplot(data=sol_agg, aes(hour,avg)) + geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + xlab(label = "Hour of Day") + ylab(label = "Output as percent of nameplate capacity") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "Average Data") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5)) + 
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,0.8)) 
dev.off()

sol_act_fin <- sol_act[1:7680,]

#solar plots
jpeg(filename = paste0(DIR,OUT,"\\images\\solar_act.jpg"), width = 500, height = 480)
ggplot(data=sol_act_fin, aes(hour,avg)) + geom_line(aes(color = max, group = id), alpha = 0.5, size = .2) + 
  theme_plot + xlab(label = "Hour of Day") + ylab(label = "Output as percent of nameplate capacity") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "October 15th, 2014 Data for 10% locations") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5)) + 
  scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,0.8)) 
dev.off()

#load plotting
load_max <- load_agg %>% group_by(id) %>% summarize(max = max(avg))
load_agg <- merge(load_agg, load_max, by="id")

load_max <- load_act %>% group_by(id) %>% summarize(max = max(avg))
load_act <- merge(load_act, load_max, by="id")


#load1
jpeg(filename = paste0(DIR,OUT,"\\images\\load_avg_BASE.jpg"), width = 500, height = 250)
ggplot(data=load_agg[which(load_agg$scen =="BASE")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + ylab(label = "Consumption (kWh)") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "Average Data: Base Case") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank()) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
dev.off()

#load2
jpeg(filename = paste0(DIR,OUT,"\\images\\load_avg_LOW.jpg"), width = 500, height = 250)
ggplot(data=load_agg[which(load_agg$scen =="LOW")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + xlab(label = "Hour of Day") + ylab(label = "Consumption (kWh)") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "Average Data: Low Case") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank()) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
dev.off()

#load3
jpeg(filename = paste0(DIR,OUT,"\\images\\load_avg_HIGH.jpg"), width = 500, height = 250)
ggplot(data=load_agg[which(load_agg$scen =="HIGH")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + xlab(label = "Hour of Day") + ylab(label = "Consumption (kWh)") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "Average Data: High Case") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5)) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
dev.off()

##ACTUAL LOAD##
#load1
jpeg(filename = paste0(DIR,OUT,"\\images\\load_act_BASE.jpg"), width = 500, height = 250)
ggplot(data=load_act[which(load_act$scen =="BASE")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + ylab(label = "Consumption (kWh)") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "October 15th, TMY3, Base") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.y=element_blank()) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
dev.off()

#load2
jpeg(filename = paste0(DIR,OUT,"\\images\\load_act_LOW.jpg"), width = 500, height = 250)
ggplot(data=load_act[which(load_act$scen =="LOW")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + ylab(label = "Consumption (kWh)") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "October 15th, TMY3, Low") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.y=element_blank()) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
dev.off()

#load2
jpeg(filename = paste0(DIR,OUT,"\\images\\load_act_HIGH.jpg"), width = 500, height = 250)
ggplot(data=load_act[which(load_act$scen =="HIGH")], aes(hr,avg/1000)) + 
  geom_line(aes(color = max, group = id), alpha = 0.4, size = .2) + 
  theme_plot + ylab(label = "Consumption (kWh)") + xlab(label = "Hour of Day") +
  scale_x_continuous(breaks=seq(0,24,2)) + ggtitle(label = "October 15th, TMY3, High") + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"), 
        legend.position = "none", plot.title = element_text(size=18,face="bold", hjust=0.5),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.y=element_blank()) + 
  scale_y_continuous(breaks=seq(0,5,1), limits=c(0,5)) 
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

#tmy_dates information
dates <- readRDS(paste0(DIR,IN,"\\tmy_dates"))
dates$month <- as.numeric(dates$month)
dates$year <- as.numeric(dates$year)
colnames(dates)[1]<-"year_tmy"

  
results_weekly <- data.frame()
results_monthly <- data.frame()
results_daily <- data.frame()
write.csv(load_sol,paste0(DIR,OUT, "\\test.csv"))

#aggregate solar/load data
for (i in 1:nrow(list)) {
  
  #get sol/stor data
  load_sol <- data.frame(readRDS(paste0(DIR,IN,"all_data_1998-2005\\LOW_", list[i,3],"_",list[i,4])))
  #data$hour <- rep(1:24,2920)
  load_sol$id <- as.numeric(list[i,10])
  
  #merging
  load_sol <- join(load_sol, dates, by = c("id","month"))
  #load_sol <- load_sol[with(load_sol, order(year.x, month, day)), ]
  load_sol$match <- ifelse(load_sol$year == load_sol$year_tmy, 1,0)
  load_sol$day_unique <- rep(x=1:2920, each=24)
  load_sol$week <- rep(x=1:418, each=168,length.out = 70080)
  load_sol$county <-as.character(list[i,3])
  load_sol$state <- as.character(list[i,4])
  
  ##daily results
  daily <- load_sol %>% dplyr::group_by(match,id,day_unique,county,state) %>% 
    dplyr::summarise(gen = sum(gen),load = sum(load))
  
  #store values
  results_daily <- rbind(results_daily,as.data.frame(daily))
  
  ##monthly results
  monthly <- load_sol %>% dplyr::group_by(match,id,month,year, county,state) %>% 
    dplyr::summarise(gen = sum(gen),load = sum(load))
  
  #store values
  results_monthly <- rbind(results_monthly,as.data.frame(monthly))
  

  ##weekly results
  weekly <- load_sol %>% dplyr::group_by(match,id,week,county,state) %>% 
    dplyr::summarise(gen = sum(gen),load = sum(load))
  
  #store values
  results_weekly <- rbind(results_weekly,as.data.frame(weekly))
  
}

# save results
save(results_weekly,results_monthly,results_daily,file=paste0(DIR,OUT,"LOW_correlation.rdata"))


#run analysis
load(file=paste0(DIR,OUT,"BASE_correlation.RData"))

#convert for plotting
results_weekly$match <- as.character(results_weekly$match)
results_monthly$match <- as.character(results_monthly$match)
results_daily$match <- as.character(results_daily$match)

#average correlation across all observations
daily_f <- results_daily %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

weekly_f <- results_weekly %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

monthly_f <- results_monthly %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

#calculations by locations
daily_t <- results_daily %>% dplyr::group_by(match,county,state) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

weekly_t <- results_weekly %>% dplyr::group_by(match,county,state) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

monthly_t <- results_monthly %>% dplyr::group_by(match,county,state) %>% 
  dplyr::summarise(correlation = cor(gen,load),count = length(gen))

#calculate overall correlation averages
check_weekly <- weekly_t %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = mean(correlation),obs = sum(count))

check_monthly <- monthly_t %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = mean(correlation, na.rm=T),obs = sum(count))  

check_daily <- daily_t %>% dplyr::group_by(match) %>% 
  dplyr::summarise(correlation = mean(correlation),obs = sum(count)) 


#graph density plots of correlation
ggplot(weekly_t, aes(correlation, colour=match, fill=match)) + 
  geom_density(alpha=0.55) +  xlab(label = "Correlation") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))

ggplot(monthly_t, aes(correlation, colour=match, fill=match)) + 
  geom_density(alpha=0.55) +  xlab(label = "Correlation") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))

ggplot(daily_t, aes(correlation, colour=match, fill=match)) + 
  geom_density(alpha=0.55) +  xlab(label = "Correlation") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.2,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))


##########################################################
## II. REgion plot #######################################
##########################################################
mapping <- fread(paste0(DIR,IN,"state_mapping.csv"))

plot_usmap(data = mapping, values = "Region", regions = "states") +  
  theme(legend.position = c(0.89,0.2),legend.text=element_text(size=16),
        legend.title=element_text(size=15,face="bold"),
        plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) +
  labs(fill="")

