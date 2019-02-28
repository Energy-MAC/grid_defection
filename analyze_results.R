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
p_load(magrittr, dplyr, stringr, ggplot2, usmap, RColorBrewer, data.table, scales,tidyr, S4Vectors)

# Set working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out"
IN = "in"
##########################################################
## I. read in output data ################################
##########################################################
#rates
base_rates_100 <- fread(paste0(DIR,OUT,"\\ann_bill_100_median.csv"))
base_rates_100$reliability <- 0
base_rates_95 <- fread(paste0(DIR,OUT,"\\ann_bill_95_median.csv"))
base_rates_95$reliability <- 0.05

base_rates <- rbind(base_rates_100, base_rates_95)

rm(base_rates_100,base_rates_95)

#solar/storage systems
sizing_1 <- fread(paste0(DIR,OUT,"\\600pv_100stor (min const).csv"))
sizing_1$version <- "600pv_100stor"

sizing_2 <- fread(paste0(DIR,OUT,"\\1200pv_400stor (min const).csv"))
sizing_2$version <- "1200pv_400stor"

#fip codes
fips <- fread(paste0(DIR,OUT,"\\fips.csv"))

##########################################################
## II. Simple graphs #####################################
##########################################################
#select analysis sample
sizing <- rbind(sizing_1,sizing_2)

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
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.9,.8)) + 
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3"),labels = c("100%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10))) + 
  scale_y_continuous(trans = 'log10') + scale_x_discrete(limits=c("LOW","BASE","HIGH"))

#breaks=seq(0,300,50), limits=c(0,300)

ggsave(filename = paste0(DIR,OUT, "\\images\\pv_sizing.jpg"))

#(storage)
ggplot(sizing, aes(x=case, y=storage, fill=reliability)) + 
  geom_boxplot() + xlab(label = "Load Case") + ylab(label = "Storage size (kWh)") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
        legend.position = c(0.9,.8)) +  
  scale_fill_manual(values = c("cadetblue4", "darkgoldenrod3"),labels = c("100%", "95%")) +
  guides(colour = guide_legend(override.aes = list(size=10)))+ 
  scale_y_continuous(trans = 'log10') + scale_x_discrete(limits=c("LOW","BASE","HIGH"))  

#breaks=seq(0,700,100), limits=c(0,700)

ggsave(filename = paste0(DIR,OUT, "\\images\\storage_sizing.jpg"))

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
                                                   mean_pv = mean(storage),
                                                   max_pv = max(pv),
                                                   min_pv = min(pv),
                                                   med_pv = median(pv),
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
  scale_fill_distiller(palette = "Spectral", limits=c(-70,1500), oob=squish, na.value="black",
                       labels = c("0","500","1000",bquote({}>=1500))) + 
    theme(legend.position = c(0.89,0.2),legend.text=element_text(size=20),
          legend.title=element_text(size=20,face="bold")) +
  labs(fill="Annual \n fixed \n  rate ($)\n")

ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index],".jpg"),width=9)

plot_usmap(data = select(base_rates,one_of(c("fips", rates[index+1]))), 
           values = rates[index+1], regions = "counties",lines=NA) + 
  scale_fill_distiller(palette = "Spectral", limits=c(0,20), oob=squish, na.value="black",
                       labels = c("0","5","10","15",bquote({}>=20))) + 
  theme(legend.position = c(0.89,0.2),legend.text=element_text(size=20),
        legend.title=element_text(size=20,face="bold")) +
  labs(fill="Variable \n rate \n (\u00A2/kWh)\n")

ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index+1],".jpg"),width=9)

}

ggplot(data = base_rates) + 
  geom_histogram(aes(x=curr_ratio, fill="Current Rates")) + 
  geom_histogram(aes(x=private_ratio, fill="PMC Rates")) +
  scale_fill_manual(values=c("aquamarine3","deeppink4")) + 
  xlab(label = "Revenue Source Ratio") + ylab(label = "Density") + 
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=20),legend.title=element_blank(),legend.position = c(0.5,0.9))  

ggsave(filename = paste0(paste0(DIR,OUT, "\\images\\density_ratio",".jpg")))

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

#bring together
sizing <- merge(sizing,opt_long_max,by=c("county","state","case"))
sizing <- merge(sizing,opt_long_energy,by=c("county","state","case"))

#bring in reliability data
reliability <- fread(paste0(DIR,OUT,"\\600pv_100stor (min const)_reliability_score.csv"))
reliability <- reliability %>% group_by(case, county,state) %>% 
  summarize(outage_max = max(count_shed))

#bring together
sizing <- merge(sizing,reliability,by=c("county","state","case"))
            
#Set costs (2 cases )
BAT_COST = 100 # $/kWh
PV_COST = 600 # $/kW
LOAD_COST = 650 # $/kW peak load
OM_COST = 100 # $/kW peak load per year

#annual rates
int_rate = 0.07 # percentage interest rate
bat_life = 20 # years
sol_life = 25 # years
inv_life = 20 # years

BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life))
INVT_RATE = int_rate / (1 - (1+int_rate)^(-inv_life))

sizing$cost <- sizing$pv * PV_COST * PV_RATE + sizing$storage * BAT_COST * BAT_RATE +
  sizing$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing$max_load

sizing$cap_ex <- sizing$pv * PV_COST  + sizing$storage * BAT_COST +
  sizing$max_load * LOAD_COST 

sizing$reliability <- as.numeric(sizing$reliability)

sizes <- merge(sizing, fips, by=c("county","state"))

##plotting system costs
#######################
loads <- c("LOW","BASE","HIGH")
rels <- c(0,0.05)

for (index in 1:length(loads)){
  
  for(rel in 1:length(rels)){
    
      size <- subset(sizes, case == loads[index] & reliability == rels[rel])
      
      plot_usmap(data = size[,c("fips","cost")], values = "cost", regions = "counties",lines=NA) + 
        scale_fill_distiller(palette = "Spectral", limits=c(0,8000), oob=squish, na.value="black",
                             labels = c("0","2000","4000","6000",bquote({}>=8000))) +
        labs(fill="Annual \n cost \n ($) \n") + 
        theme(legend.position = c(0.89,0.2),legend.text=element_text(size=20),
              legend.title=element_text(size=20,face="bold"))
      
      ggsave(filename = paste0(DIR,OUT, "\\images\\syscost_",rels[rel],"_",loads[index],".jpg"))
  }
}


##plotting grid defection
#######################

##merge in data
defect <- merge(base_rates, sizing[,c(1:3,5,6,8:13)], by=c("county","state","case", "reliability"), all.x = TRUE)

#take differences
defect$curr_diff <- defect$r_current - defect$cost
defect$private_diff <- defect$r_private - defect$cost
defect$r1_diff <- defect$r_1 - defect$cost
defect$r0.75_diff <- defect$r_0.75 - defect$cost
defect$r0.5_diff <- defect$r_0.5 - defect$cost
defect$r0.25_diff <- defect$r_0.25 - defect$cost
defect$r0_diff <- defect$r_0 - defect$cost
defect$current <- as.character(ifelse(defect$curr_diff > 0 ,1,0))
defect$private <- as.character(ifelse(defect$private_diff > 0 ,1,0))
#write to csv
write.csv(defect,file = paste0(DIR,OUT, "\\defection_v3.csv"))


loads <- c("LOW","BASE","HIGH")
rate <- c("private","current")
rels <- c(0,0.05)

for (index in 1:length(loads)){
  for(rel in 1:length(rels)){
    
    final <- subset(defect, case == loads[index] & reliability == rels[rel])
    
    for (ra in 1:length(rate)){  
      
      data <- select(final,one_of(c("fips", rate[ra])))
      
     if(nrow(unique(data[,2]))>1){
       cols <- c("deeppink4","lightgoldenrod3")
      } else {
       cols <- c("deeppink4")
      }
      
      if(nrow(unique(data[,2]))>1){
        labs <- c("no defection","defection")
      } else {
        labs <- c("no defection")
      } 
      
      plot_usmap(data = data, 
                 values = rate[ra], regions = "counties", lines=NA) +  
         scale_fill_manual(values=cols,labels=labs,na.value="black") +
        theme(legend.position = c(0.89,0.2),legend.text=element_text(size=18),
              legend.title=element_text(size=15,face="bold"),
              plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) +
        labs(fill="")
      
      ggsave(filename = paste0(DIR,OUT, "\\images\\outcome_",rate[ra],"_",
                               loads[index],"_",rels[rel],".jpg"))
    }
  }
}



## density plots
rate <- c("private_diff","curr_diff")
rels <- c(0,0.05)

for (index in 1:length(rate)){
  for(rel in 1:length(rels)){
    
    ggplot(defect[reliability==rels[rel]], aes_string(rate[index], colour="case", fill="case")) + 
      geom_density(alpha=0.55) + geom_vline(xintercept=0) +
      xlab(label = "solar/storage system cost savings $") + ylab(label = "Density") + 
      theme(axis.text=element_text(size=18),axis.title=element_text(size=20,face="bold"), 
            legend.text=element_text(size=20),legend.title=element_text(size=20,face="bold"),
            legend.position = c(0.2,0.8)) + 
      guides(colour = guide_legend(override.aes = list(size=10))) + 
      scale_x_continuous(limits=c(-15000,1500), labels=dollar_format(prefix = "$")) + 
      scale_y_continuous(limits=c(0,0.002))
    
    ggsave(filename = paste0(paste0(DIR,OUT, "\\images\\density_",rate[index],"_",rels[rel],".jpg")))
    
  }
}

##########################################################
## III. Sizes vs. cost assumptions #######################
##########################################################
#get costs
#Set costs
BAT_COST = 100 # $/kWh
PV_COST = 600 # $/kW
LOAD_COST = 800 # $/kW peak load
OM_COST = 100 # $/kW peak load per year

sizing_1 <- merge(sizing_1,opt_long_max,by=c("county","state","case"))
sizing_1$cost <- sizing_1$pv * PV_COST * PV_RATE + sizing_1$storage * BAT_COST * BAT_RATE +
  sizing_1$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing_1$max_load

#get costs
#Set costs
BAT_COST = 400 # $/kWh
PV_COST = 1200 # $/kW
LOAD_COST = 1600 # $/kW peak load
OM_COST = 100 # $/kW peak load per year

sizing_2 <- merge(sizing_2,opt_long_max,by=c("county","state","case"))
sizing_2$cost <- sizing_2$pv * PV_COST * PV_RATE + sizing_2$storage * BAT_COST * BAT_RATE +
  sizing_2$max_load * LOAD_COST * INVT_RATE + OM_COST * sizing_2$max_load

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
                                                                  stor_90 = quantile(storage,.9))

write.csv(r_stats,file = paste0(DIR,OUT, "\\size_cost_v3.csv"))


##########################################################
## III. Analyze load shedding ############################
##########################################################
DIR2 <- "G:\\Team Drives\\grid_defect_data\\Analysis\\"
#DIR2 <- "C:\\Users\\Will\\Desktop\\data\\Analysis\\"

folder <- "\\600pv_100stor (min const)"

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
  
  final <- out %>% group_by(reliability, case, county,state,count_shed,month) %>% 
    summarize(load = sum(load),
              shed = sum(shed))
  
  results <- rbind(results,as.data.frame(final))
}

write.csv(results, file = paste0(DIR2,OUT, "\\","600pv_100stor (min const)_month_shedding.csv"))


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

write.csv(results, file = paste0(DIR2,OUT, "\\","600pv_100stor (min const)_reliability_score_v2.csv"))

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
