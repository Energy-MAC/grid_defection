###############################################################################
###
###   Off-grid Analysis
###   PURPOSE: plot optimization results
###
###############################################################################

# Clear workspace
rm(list = ls())

# Packages
library(pacman)
p_load(magrittr, dplyr, stringr, ggplot2, usmap, 
       RColorBrewer, data.table, scales,tidyr, S4Vectors)

# Set working directory
DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
#DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out"
IN = "in"
##########################################################
## I. read in output data ################################
##########################################################
#rates
base_rates_100 <- fread(paste0(DIR,OUT,"\\ann_bill_100_median.csv"))
base_rates_100$reliability <- 0
base_rates_99 <- fread(paste0(DIR,OUT,"\\ann_bill_99_median.csv"))
base_rates_99$reliability <- 0.01

base_rates <- rbind(base_rates_100, base_rates_99)

rm(base_rates_100,base_rates_99)

#solar/storage systems
sizing_1 <- fread(paste0(DIR,OUT,"\\600pv_100stor (min const @4% DR).csv"))
sizing_1$version <- "600pv_100stor"

sizing_2 <- fread(paste0(DIR,OUT,"\\1200pv_400stor (min const @4% DR).csv"))
sizing_2$version <- "1200pv_400stor"

#fip codes
fips <- fread(paste0(DIR,OUT,"\\fips.csv"))

##########################################################
## II. Simple graphs #####################################
##########################################################
#select analysis sample
sizing <- rbind(sizing_1,sizing_2)
sizing$reliability <- as.character(sizing$reliability)

##plotting reliability of 0
ggplot(data=sizing[reliability=="0"], aes(pv,storage)) + geom_point(aes(color=case)) +
  xlab(label = "Solar size (kW)") + ylab(label = "Storage size (kWh)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold")) + 
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_x_continuous(breaks=seq(0,450,50), limits=c(0,450)) + 
  scale_y_continuous(breaks=seq(0,2000,500), limits=c(0,2000)) 

ggsave(filename = paste0(DIR,OUT, "\\images\\sizing_reliability_0.jpg"))

##plotting reliability of 0.05
ggplot(data=sizing[reliability=="0.05"], aes(pv,storage)) + geom_point(aes(color=case)) +
  xlab(label = "Solar size (kW)") + ylab(label = "Storage size (kWh)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold")) + 
  guides(colour = guide_legend(override.aes = list(size=10))) +
  scale_x_continuous(breaks=seq(0,450,50), limits=c(0,450)) + 
  scale_y_continuous(breaks=seq(0,2000,500), limits=c(0,2000)) 

ggsave(filename = paste0(DIR,OUT, "\\images\\sizing_reliability_0.05.jpg"))

##box and whisker (solar)
  ggplot(sizing, aes(x=case, y=pv, fill=reliability)) + 
    geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Solar size (kW)") + 
    geom_hline(yintercept = 20, linetype="dashed",color="red") + 
    annotate("text",x = 3, y=9,label="capacity constraint (20 kW)", color = "red") +
    theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
          legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
          legend.position = c(0.15,.88)) + 
    scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3", "red"),labels = c("100%","99%", "95%")) +
    guides(colour = guide_legend(override.aes = list(size=10))) + 
    scale_y_continuous(trans = 'log10') + scale_x_discrete(limits=c("LOW","BASE","HIGH")) 

#breaks=seq(0,300,50), limits=c(0,300)

ggsave(filename = paste0(DIR,OUT, "\\images\\pv_sizing.jpg"),width = 6, height = 5)

#(storage)
ggplot(sizing, aes(x=case, y=storage, fill=reliability)) + 
  geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Storage size (kWh)") +
  geom_hline(yintercept = 84, linetype="dashed",color="red") + 
  annotate("text",x = 2.9, y=20,label="capacity constraint (84 kWh)", color = "red") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.15,.85)) +  
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3", "red"),labels = c("100%", "99%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10)))+ 
  scale_y_continuous(trans = 'log10') + scale_x_discrete(limits=c("LOW","BASE","HIGH"))  

#breaks=seq(0,700,100), limits=c(0,700)

ggsave(filename = paste0(DIR,OUT, "\\images\\storage_sizing.jpg"),width = 6, height = 5)

##comparing reliability difference
merged <- merge(sizing_3[,c(2,3,5:8)], sizing_4[,c(2,3,5:8)], by =c("reliability","case","county","state"))
merged$solar_d <- merged$pv.x - merged$pv.y
merged$stor_d <- merged$storage.x - merged$storage.y

##box and whisker (solar)
ggplot(merged, aes(x=case, y=solar_d, fill=reliability)) + 
  geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Solar size difference (kW)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.2,.2)) + 
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3"),labels = c("100%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10))) 

ggsave(filename = paste0(DIR,OUT, "\\images\\pv_sizing_diff.jpg"))

#(storage)
ggplot(merged, aes(x=case, y=stor_d, fill=reliability)) + 
  geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Storage size difference (kW)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.2,.2)) +  
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3"),labels = c("100%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10))) 

ggsave(filename = paste0(DIR,OUT, "\\images\\storage_sizing_diff.jpg"))

##average stats (by load)

l_stats <- sizing %>% group_by(case) %>% summarize(max_pv = max(pv),
                                                   min_pv = min(pv),
                                                   max_stor = max(storage),
                                                   min_stor = min(storage))

##average stats (by reliability)
r_stats <- sizing %>% group_by(reliability) %>% summarize(mean_sol = mean(pv),
                                                   max_pv = max(pv),
                                                   min_pv = min(pv),
                                                   med_pv = median(pv),
                                                   mean_stor = mean(storage),
                                                   max_stor = max(storage),
                                                   min_stor = min(storage),
                                                   med_stor = median(storage))

##ratio analysis
sizing$ratio <- sizing$storage/sizing$pv
test <- reshape(sizing[,c(2,3,5:9)], idvar = c("case","county","state"), 
                timevar="reliability", v.names=c("ratio","pv","storage"), direction="wide")
test$ratio_diff <- test$ratio.0.05 - test$ratio.0

##plot ratios
ggplot(test, aes(x=case, y=ratio_diff)) + 
  geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Solar size (kW)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.7,.8)) + 
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3"),labels = c("100%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10))) #+ 
#scale_y_continuous(breaks=seq(0,400,50), limits=c(0,400))


##########################################################
## III. Geospatial Analysis of rates #####################
##########################################################

rates <- c("r_curr_fixed","r_curr_variable", "private_fixed","private_variable")
ratio <- c("curr_ratio","private_ratio")

for (index in seq(1, 4, 2)){

plot_usmap(data = select(base_rates,one_of(c("fips", rates[index]))), 
           values = rates[index], regions = "counties",lines=NA) + 
  scale_fill_distiller(palette = "Blues", direction = 1, limits=c(-70,1500), oob=squish, 
                       na.value="black",labels = c("0","500","1000",bquote({}>=1500))) + 
    theme(legend.position = c(0.92,0.2),legend.text=element_text(size=20),
          legend.title=element_text(size=20,face="bold")) +
  labs(fill="Annual \n fixed \n  rate ($)\n")
  
#Spectral was original scale
  
ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index],".jpg"),width=11)

plot_usmap(data = select(base_rates,one_of(c("fips", rates[index+1]))), 
           values = rates[index+1], regions = "counties",lines=NA) + 
  scale_fill_distiller(palette = "Greens", direction = 1, limits=c(0,20), oob=squish, 
                       na.value="black", labels = c("0","5","10","15",bquote({}>=20))) + 
  theme(legend.position = c(0.92,0.2),legend.text=element_text(size=20),
        legend.title=element_text(size=20,face="bold")) +
  labs(fill="Variable \n rate \n (\u00A2/kWh)\n")

ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index+1],".jpg"),width=11)

}

ggplot(data = base_rates) + 
  geom_histogram(aes(x=curr_ratio, fill="Current Rates")) + 
  geom_histogram(aes(x=private_ratio, fill="PMC Rates")) +
  scale_fill_manual(values=c("aquamarine3","deeppink4")) + 
  xlab(label = "% recovery via fixed charges") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=20),legend.title=element_blank(),legend.position = c(0.5,0.9))  

ggsave(filename = paste0(DIR,OUT, "\\images\\density_ratio",".jpg"),width=11)

##########################################################
## III. Geospatial Analysis of costs #####################
##########################################################
#select analysis sample
sizing <- sizing_1

#bring in load data
opt <- fread(paste0(DIR,IN,"\\optimization_list_energy.csv"))
opt_long_max <- gather(opt[,c(4:5,17:19)],key=case,value=max_load,low_max:high_max)
opt_long_max$case <- ifelse(opt_long_max$case == "low_max","LOW",
                        ifelse(opt_long_max$case == "med_max","BASE","HIGH"))
#energy data
opt_long_energy <- gather(opt[,c(4:5,14:16)],key=case,value=energy,low_energy:high_energy)
opt_long_energy$case <- ifelse(opt_long_energy$case == "low_energy","LOW",
                            ifelse(opt_long_energy$case == "med_energy","BASE","HIGH"))

#solar data
sol_col <- fread(paste0(DIR,IN,"\\solar_collection.csv"))

#bring together
sizing <- merge(sizing,opt_long_max,by=c("county","state","case"))
sizing <- merge(sizing,opt_long_energy,by=c("county","state","case"))
sizing <- merge(sizing,sol_col[,3:5],by=c("county","state"))

#bring in reliability data
reliability <- fread(paste0(DIR,OUT,"\\600pv_100stor (min const 4% DR)_reliability_score.csv"))
reliability <- reliability %>% group_by(case, county,state) %>% 
  summarize(outage_max = max(count_shed))

#bring together
sizing <- merge(sizing,reliability,by=c("county","state","case"))
            
#Set costs (2 cases )
BAT_COST = 100 # $/kWh
PV_COST = 600 # $/kW
LOAD_COST = 650 # $/kW peak load
OM_COST = 12 # $/kW peak load per year

#annual rates
int_rate = 0.04 # percentage interest rate
bat_life = 10 # years
sol_life = 25 # years
inv_life = 10 # years

BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life))
INVT_RATE = int_rate / (1 - (1+int_rate)^(-inv_life))

sizing$grid_defect_cost <- sizing$pv * PV_COST * PV_RATE + sizing$storage * BAT_COST * BAT_RATE +
  sizing$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing$max_load

sizing$pv_lev <- (PV_COST * PV_RATE + 
                     LOAD_COST * INVT_RATE + 
                    OM_COST ) / (sizing$gen/9)

sizing$pv_size_load_offset <- sizing$energy / (sizing$gen/9)

sizing$cap_ex <- sizing$pv * PV_COST  + sizing$storage * BAT_COST +
  sizing$max_load * LOAD_COST 

sizing$reliability <- as.numeric(sizing$reliability)

sizes <- merge(sizing, fips, by=c("county","state"))

##plotting system costs
#######################
loads <- c("LOW","BASE","HIGH")
rels <- c(0,0.01)

for (index in 1:length(loads)){
  
  for(rel in 1:length(rels)){
    
      size <- subset(sizes, case == loads[index] & reliability == rels[rel])
      
      plot_usmap(data = size[,c("fips","grid_defect_cost")], values = "grid_defect_cost", regions = "counties",lines=NA) + 
        scale_fill_distiller(palette = "Spectral", limits=c(0,8000), oob=squish, na.value="black",
                             labels = c("0","2000","4000","6000",bquote({}>=8000))) +
        labs(fill="Annual \n cost \n ($) \n") + 
        theme(legend.position = c(0.89,0.2),legend.text=element_text(size=20),
              legend.title=element_text(size=20,face="bold"))
      
      ggsave(filename = paste0(DIR,OUT, 
                               "\\images\\syscost_",rels[rel],"_",loads[index],".jpg"),width=13)
  }
}


##plotting grid defection
#######################

##merge in data
defect <- merge(base_rates, sizing[,c(1:3,5:17)], by=c("county","state","case", "reliability"), all.x = TRUE)

##load defection cost
defect$load_defect_cost_current <- defect$r_curr_fixed + (1 - defect$reliability) * defect$energy * 
  defect$pv_lev
  
defect$load_defect_cost_pmc <- defect$private_fixed + (1 - defect$reliability) * defect$energy * 
  defect$pv_lev

#take differences
defect$curr_diff <- defect$r_current - defect$grid_defect_cost
defect$private_diff <- defect$r_private - defect$grid_defect_cost

##cost based defection
defect$current_defect_cost <- as.character(ifelse(defect$r_current < defect$load_defect_cost_current & 
                                      defect$r_current < defect$grid_defect_cost,0,
                                  ifelse(defect$load_defect_cost_current < defect$grid_defect_cost,1,2)))

defect$private_defect_cost <- as.character(ifelse(defect$r_private < defect$load_defect_cost_pmc & 
                                      defect$r_private < defect$grid_defect_cost,0,
                                  ifelse(defect$load_defect_cost_pmc < defect$grid_defect_cost,1,2)))
##with system constraints
defect$current_defect_wconst <- as.character(ifelse(defect$current_defect_cost == 2 & 
          (defect$pv > 20 | defect$storage > 200) & defect$r_current < defect$load_defect_cost_current,0,
          ifelse(defect$current_defect_cost == 2 & (defect$pv > 20 | defect$storage > 200) & 
              defect$r_current > defect$load_defect_cost_current,1,defect$current_defect_cost)))

defect$private_defect_wconst <- as.character(ifelse(defect$private_defect_cost == 2 & 
          (defect$pv > 20 | defect$storage > 200) & defect$r_private < defect$load_defect_cost_pmc,0,
        ifelse(defect$private_defect_cost == 2 & (defect$pv > 20 | defect$storage > 200) & 
            defect$r_private > defect$load_defect_cost_pmc,1,defect$private_defect_cost)))

#write to csv
write.csv(defect,file = paste0(DIR,OUT, "\\defection_v6_highcost.csv"))

## Grid defection plotting
loads <- c("LOW","BASE","HIGH")
rate <- c("private_defect_wconst","current_defect_wconst")
rels <- c(0,0.01)

for (index in 1:length(loads)){
  for(rel in 1:length(rels)){
    
    final <- subset(defect, case == loads[index] & reliability == rels[rel])
    
    for (ra in 1:length(rate)){  
      
      data <- select(final,one_of(c("fips", rate[ra])))
      data$color <- ifelse(data[,2]=="0","gray",ifelse(data[,2]=="1","deeppink4","darkolivegreen"))
      data$label <- ifelse(data[,2]=="0","no defection",ifelse(data[,2]=="1","load defection","grid defection"))
      data <- data[order(-label),]
      
      cols <- unique(data[,3])
      cols <- as.matrix(cols)
      labs <- unique(data[,4])
      labs <- as.matrix(labs)

      plot_usmap(data = data, 
                 values = rate[ra], regions = "counties", lines=NA) +  
         scale_fill_manual(values=cols,labels=labs,na.value="black") +
        theme(legend.position = c(0.85,0.2),legend.text=element_text(size=28),
              legend.title=element_text(size=24,face="bold"),
              plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) + guides(fill=FALSE) +
        labs(fill="")
      
      ggsave(filename = paste0(DIR,OUT, "\\images\\PR\\outcome_",rate[ra],"_",
                               loads[index],"_",rels[rel],".jpg"),width=13)
    }
  }
}


## density plots
rels <- c(0,0.01)

for(rel in 1:length(rels)){
  
  temp1 <- subset(defect[reliability==rels[rel]], case == "BASE")
  row1 <- as.numeric(nrow(temp1))
  temp2 <- subset(defect[reliability==rels[rel]], case == "LOW")
  row2 <- as.numeric(nrow(temp2))
  temp3 <- subset(defect[reliability==rels[rel]], case == "HIGH")
  row3 <- as.numeric(nrow(temp3))
    
  ggplot() + 
    geom_area(data = temp1, stat="bin", aes(x = private_diff,y=..count.., 
                                            fill="Base"), alpha=0.55) + 
    geom_area(data = temp2, stat="bin", aes(x = private_diff, y=..count.., 
                                            fill="Low"), alpha=0.55) + 
    geom_area(data = temp3, stat="bin", aes(x = private_diff,y=..count..,  
                                            fill="High"), alpha=0.55) + 
    scale_fill_manual(values=c("aquamarine3","deeppink4", "gold")) +
    geom_vline(xintercept=0) + labs(fill="Load case") + 
    xlab(label = "solar/storage system cost savings $") + ylab(label = "Count (# counties)") + 
    theme(axis.text=element_text(size=22),axis.title=element_text(size=24,face="bold"), 
          legend.text=element_text(size=24),legend.title=element_text(size=24,face="bold"),
          legend.position = c(0.2,0.8), legend.key.size =  unit(1, 'cm')) + 
    guides(colour = guide_legend(override.aes = list(size=14))) + 
    scale_x_continuous(limits=c(-15000,1500), labels=dollar_format(prefix = "$")) + 
    scale_y_continuous(limits=c(0,2000))
  
  ggsave(filename = paste0(DIR,OUT, "\\images\\density_private_diff_",rels[rel],".jpg"),width=10)
    
  ggplot() + 
    geom_area(data = temp1, stat="bin", aes(x = curr_diff,y=..count.., 
                                            fill="Base"), alpha=0.55) + 
    geom_area(data = temp2, stat="bin", aes(x = curr_diff, y=..count.., 
                                            fill="Low"), alpha=0.55) + 
    geom_area(data = temp3, stat="bin", aes(x = curr_diff,y=..count..,  
                                            fill="High"), alpha=0.55) + 
    scale_fill_manual(values=c("aquamarine3","deeppink4", "gold")) +
    geom_vline(xintercept=0) + labs(fill="Load case") + 
    xlab(label = "solar/storage system cost savings $") + ylab(label = "Count (# counties)") + 
    theme(axis.text=element_text(size=22),axis.title=element_text(size=24,face="bold"), 
          legend.text=element_text(size=24),legend.title=element_text(size=24,face="bold"),
          legend.position = c(0.2,0.8), legend.key.size =  unit(1, 'cm')) + 
    guides(colour = guide_legend(override.aes = list(size=14))) + 
    scale_x_continuous(limits=c(-15000,1500), labels=dollar_format(prefix = "$")) + 
    scale_y_continuous(limits=c(0,2000))
  
  ggsave(filename = paste0(DIR,OUT, "\\images\\density_curr_diff_",rels[rel],".jpg"),width=10)
}


##########################################################
## III. Sizes vs. cost assumptions #######################
##########################################################
#get costs
#Set costs
BAT_COST = 100 # $/kWh
PV_COST = 600 # $/kW
LOAD_COST = 650 # $/kW peak load
OM_COST = 12 # $/kW peak load per year

sizing_1 <- merge(sizing_1,opt_long_max,by=c("county","state","case"))
sizing_1$cost <- sizing_1$pv * PV_COST * PV_RATE + sizing_1$storage * BAT_COST * BAT_RATE +
  sizing_1$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing_1$max_load

sizing_1 <- merge(sizing_1,opt_long_energy,by=c("county","state","case"))

#get costs
#Set costs
BAT_COST = 400 # $/kWh
PV_COST = 1200 # $/kW
LOAD_COST = 1300 # $/kW peak load
OM_COST = 12 # $/kW peak load per year

sizing_2 <- merge(sizing_2,opt_long_max,by=c("county","state","case"))
sizing_2$cost <- sizing_2$pv * PV_COST * PV_RATE + sizing_2$storage * BAT_COST * BAT_RATE +
  sizing_2$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing_2$max_load

sizing_2 <- merge(sizing_2,opt_long_energy,by=c("county","state","case"))

compiled <- rbind(sizing_1, sizing_2)

##average stats
r_stats <- sizing_1 %>% group_by(reliability, case) %>% summarize(med_sol = median(pv),
                                                                  med_stor = median(storage),
                                                                  max_sol = max(pv), 
                                                                  max_stor = max(storage), 
                                                                  min_sol = min(pv),
                                                                  min_stor = min(storage), 
                                                                  med_cost = median(cost),
                                                                  count = length(pv),
                                                                  mean_sol = mean(pv),
                                                                  mean_stor = mean(storage),
                                                                  pv_10 = quantile(pv,.1),
                                                                  stor_10 = quantile(storage,.1),
                                                                  pv_90 = quantile(pv,.9),
                                                                  stor_90 = quantile(storage,.9),
                                                                  med_energy = median(energy))

write.csv(r_stats,file = paste0(DIR,OUT, "\\size_cost_v5.csv"))

##########################################################
## III. Analyze load shedding ############################
##########################################################
DIR2 <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
#DIR2 <- "C:\\Users\\Will\\Desktop\\data\\Analysis\\"

folder <- "\\600pv_100stor (min const @4% DR)"

list <- list.files(paste0(DIR2,OUT,folder,"\\"))
list <- list[lapply(list,function(x) length(grep("results",x,value=FALSE))) == 0]
list <- list[lapply(list,function(x) length(grep("_0_",x,value=FALSE))) == 0]
results <- data.frame()

##summing when shedding happens
for (i in 1:length(list)) {
  
  out <- fread(paste0(DIR2, OUT,folder,"\\",list[i]))
  out$id <- substr(list[i],1,nchar(list[i]) - 4)
  out <- out %>% separate(id, c("remove","reliability","case","county","state"), "_")
  out$remove <- NULL
  out$month <- rep(x=1:12, each=730, length.out=78840)
  out$year <-  rep(x=2008:2016, each=8760, length.out=78840)
  
  final <- out %>% group_by(reliability, case, county,state,month,year) %>% 
    summarize(load = sum(load),
              shed = sum(shed))
  
  results <- rbind(results,as.data.frame(final))
}

write.csv(results, file = paste0(DIR2,OUT, "\\","600pv_100stor (min const 4% DR)_month_shedding.csv"))

#create output for graph
results<- fread(paste0(DIR,OUT,"\\600pv_100stor (min const 4% DR)_month_shedding.csv"))
results$shed_percent <- results$shed / results$load
results$reliability <- as.numeric(results$reliability)

results_final <- merge(results, defect[,c(1:4,32:33)], by=c("county","state","case", "reliability"), 
                       all.x = TRUE)

final <- results_final %>% group_by(month,private_defect_wconst) %>% 
  summarize(shed_percent = mean(shed_percent),
            count = length(county))

write.csv(final, file = paste0(DIR,OUT, "\\month_shedding_private.csv"))

final_yr <- results %>% group_by(month,year) %>% 
  summarize(shed = sum(shed),
            load = sum(load))

write.csv(final_yr, file = paste0(DIR,OUT, "\\year_shedding_private.csv"))

final_yr$shed_percent <- final_yr$shed / final_yr$load
###########################
results <- data.frame()

##counting length of reliability
for (i in 1:length(list)) {
  
  out <- fread(paste0(DIR2, OUT,folder,"\\",list[i]))
  out$id <- substr(list[i],1,nchar(list[i]) - 4)
  out <- out %>% separate(id, c("remove","reliability","case","county","state"), "_")
  out$remove <- NULL
  vector <- ifelse(out$shed>0,1,0)
  value <- numeric(length(vector))
  
  for(v in 1:length(vector)) {
    if(v==1){
        value[v] = 0
      }else if(vector[v]==1){
        value[v] = value[v-1]+1
      }else{
        value[v] = 0
      }
  }
  
  out$count_shed <- value
  out$month <- rep(x=1:12, each=730, length.out=78840)
  
  final <- out %>% group_by(reliability, case, county,state,count_shed) %>% 
    summarize(tot = length(load),
              month = mean(month))
  
  results <- rbind(results,as.data.frame(final))
}

write.csv(results, file = paste0(DIR2,OUT, "\\","600pv_100stor (min const 4% DR)_reliability_score.csv"))

#load in reliability results
results<- fread(paste0(DIR,OUT,"\\600pv_100stor (min const)_reliability_score_v2.csv"))
results <- results %>% group_by(reliability,case,county,state) %>% 
  mutate(diff = tot - lead(tot, default = 0))
results <- final[final$diff != 0,]


## distribution of month of outage
timing <- results %>% group_by(reliability, month,case, count_shed) %>% summarise(tot = sum(diff))

plot_1 <- results[results$count_shed <24,]
plot_1<- plot_1[rep(row.names(plot_1), plot_1$diff), 1:9]
ggplot(plot_1, aes(month, colour=case, fill=case)) + 
  geom_density(alpha=0.55) + xlab(label = "Month") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.2,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10))) + 
  scale_x_continuous(breaks = seq(1,12,1))


## plotting different outage lengths
final <- results %>% group_by(reliability, count_shed,case) %>% summarise(tot = sum(diff))

full <- final[final$count_shed >0,]
ggplot(full, aes(x = count_shed, y=tot, fill=case)) + 
  geom_bar(stat="identity")  + xlab(label = "outage length (hrs)") + ylab(label = "Count") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+ 
  scale_x_continuous(limits=c(0,100))

ggsave(filename = paste0(DIR,OUT, "\\images\\full_outage.jpg"))

reduce <- final[final$count_shed >12,]
ggplot(reduce, aes(x = count_shed, y=tot, fill=case)) + 
  geom_bar(stat="identity")  + xlab(label = "outage length (hrs)") + ylab(label = "Count") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+ 
  scale_x_continuous(limits=c(0,500))

ggsave(filename = paste0(DIR,OUT, "\\images\\reduce_outage.jpg"))

reduce <- final[final$count_shed >25,]
ggplot(reduce, aes(x = count_shed, y=tot, fill=case)) + 
  geom_bar(stat="identity")  + xlab(label = "outage length (hrs)") + ylab(label = "Count") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.8,0.8)) + 
  guides(colour = guide_legend(override.aes = list(size=10)))+ 
  scale_x_continuous(limits=c(0,500))

ggsave(filename = paste0(DIR,OUT, "\\images\\more_reduce_outage.jpg"))
