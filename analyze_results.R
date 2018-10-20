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
p_load(magrittr, dplyr, stringr, ggplot2, usmap, RColorBrewer, data.table, scales)

# Set working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\Research\\Papers\\2018 Off-grid\\Analysis\\"
OUT = "out"
INPUT = "in"
##########################################################
## I. read in output data ################################
##########################################################
#rates
base_rates_100 <- fread(paste0(DIR,OUT,"\\ann_bill_100.csv"))
base_rates_100$reliability <- 0
base_rates_95 <- fread(paste0(DIR,OUT,"\\ann_bill_100.csv"))
base_rates_95$reliability <- 0.05

base_rates <- rbind(base_rates_100, base_rates_95)

rm(base_rates_100,base_rates_95)

#solar/storage systems
sizing <- fread(paste0(DIR,OUT,"\\500pv_100stor.csv"))
fips <- fread(paste0(DIR,OUT,"\\fips.csv"))


##########################################################
## II. Simple graphs ######################################
##########################################################

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

##average stats (by load)

l_stats <- sizing %>% group_by(case) %>% summarize(max_pv = max(pv),
                                                   min_pv = min(pv),
                                                   max_stor = max(storage),
                                                   min_stor = min(storage))

##average stats (by reliability)
r_stats <- sizing %>% group_by(reliability) %>% summarize(mean_sol = mean(pv),
                                                   mean_pv = mean(storage))

##########################################################
## III. Geospatial Analysis of rates #####################
##########################################################

rates <- c("r_curr_fixed","r_curr_variable", "r_1_fixed","r_0_variable")

for (index in seq(1, 4, 2)){

name <- c("fips",rates[index]) 

plot_usmap(data = select(base_rates,one_of(c("fips", rates[index]))), 
           values = rates[index], regions = "counties") + 
  scale_fill_distiller(palette = "Spectral", limits=c(-70,2400), na.value="black") + 
    theme(legend.position = c(0.89,0.2),legend.text=element_text(size=15),
          legend.title=element_text(size=15,face="bold"),
          plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) +
  labs(fill="Annual fixed rate ($)")

ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index],".jpg"))

plot_usmap(data = select(base_rates,one_of(c("fips", rates[index+1]))), 
           values = rates[index+1], regions = "counties") + 
  scale_fill_distiller(palette = "Spectral", limits=c(2.5,30), na.value="black") + 
  theme(legend.position = c(0.89,0.2),legend.text=element_text(size=15),
        legend.title=element_text(size=15,face="bold"),
        plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) +
  labs(fill="Variable rate \n (cent/kWh)")

ggsave(filename = paste0(DIR,OUT, "\\images\\",rates[index+1],".jpg"))

}

##########################################################
## III. Geospatial Analysis of sizes #####################
##########################################################
#Set costs
BAT_COST = 200 # $/kWh
PV_COST = 1000 # $/kW

#annual rates
int_rate = 0.07 # percentage interest rate
bat_life = 20 # years
sol_life = 25 # years

BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life))

sizing$cost <- sizing$pv * PV_COST * PV_RATE + sizing$storage * BAT_COST * BAT_RATE
sizing$reliability <- as.numeric(sizing$reliability)

sizes <- merge(sizing, fips, by=c("county","state"))

##plotting system costs
#######################
loads <- c("LOW","BASE","HIGH")
rels <- c(0,0.05)

for (index in 1:length(loads)){
  
  for(rel in 1:length(rels)){
    
      size <- subset(sizes, case == loads[index] & reliability == rels[rel])
      
      plot_usmap(data = size[,c("fips","cost")], values = "cost", regions = "counties") + 
        scale_fill_distiller(palette = "Spectral", limits=c(0,20000), oob=squish, na.value="black",
                             labels = c("0","5000","10000","15000",bquote({}>=20000))) +
        labs(fill="Annual cost ($)") + 
        theme(legend.position = c(0.89,0.2),legend.text=element_text(size=15),
              legend.title=element_text(size=15,face="bold"),
              plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0))
      
      ggsave(filename = paste0(DIR,OUT, "\\images\\syscost_",rels[rel],"_",loads[index],".jpg"))
  }
}


##plotting grid defection
#######################

##merge in data
defect <- merge(base_rates, sizing[,c(5:9)], by=c("county","state","case", "reliability"), all.x = TRUE)

#take differences
defect$curr_diff <- defect$r_current - defect$cost
defect$r1_diff <- defect$r_1 - defect$cost


loads <- c("LOW","BASE","HIGH")
rate <- c("r1_diff","curr_diff")
rels <- c(0,0.05)

for (index in 1:length(loads)){
  for(rel in 1:length(rels)){
    
    final <- subset(defect, case == loads[index] & reliability == rels[rel])
    
    for (ra in 1:length(rate)){  
      
      plot_usmap(data = select(final,one_of(c("fips", rate[ra]))), 
                 values = rate[ra], regions = "counties") + 
        scale_fill_distiller(palette = "Spectral", limits=c(-2000,2000), oob=squish, na.value="black",
                             labels = c(bquote({}<=-2000),"-1000","0","1000","2000")) + 
        theme(legend.position = c(0.89,0.2),legend.text=element_text(size=15),
              legend.title=element_text(size=15,face="bold"),
              plot.title = element_text(size=18,face="bold", hjust=0.5, vjust=0)) +
        labs(fill="Annual \n difference ($)")
      

      ggsave(filename = paste0(DIR,OUT, "\\images\\outcome_",rate[ra],"_",
                               loads[index],"_",rels[rel],".jpg"))
      
    }
  }
}
