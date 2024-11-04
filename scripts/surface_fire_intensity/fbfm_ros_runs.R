#####
#
## using Rothermel package to find ROS for different wind, fuel moisture scenarios
#
#####

rm(list=ls())

# libraries
library(Rothermel)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

#####
# 1. determine range of windspeeds from RAWS data
#####

# load old faithful raws

wind.in = read.table(file="data/RAWS/OldFaithfulWind_DataOnly.txt", header=TRUE) %>%
  separate(Date, c("Month","Day","Year"), sep="/")  # separate date into ymd

# subset to complete wind gust data
# only fire season, july through sept
# change to km/h
wind.sub = wind.in %>%
  filter(Wind_Speed_Gust_mps != -9999, Month %in% c("07","08","09")) %>%
  mutate(wind_speed_kmph = Wind_Speed_Gust_mps * 0.001 * 3600)

quantile(wind.sub$wind_speed_kmph,probs=c(0.01,0.99)) # 14.05 to 60.74 km/hr

# using 15 to 60 km/hr, rounded to nearest 5 for wind calcs.

#####
# 2. determine which FBFMs to use
#####

# import LANDFIRE data, clipped to approx boundaries of GYE
fbfm.in = raster("data/LANDFIRE/US_200FBFM40/us_200fbfm40/hdr.adf")
evt.in = raster("data/LANDFIRE/US_200EVT/us_200evt/hdr.adf")

lf.rast = stack(fbfm.in, evt.in)

# extract values and match up with attributes
lf.dat = data.frame(rasterToPoints(lf.rast))

lf.dat2 = fbfm.in@data@attributes[[1]] %>%
  dplyr::select(ID,FBFM40) %>%
  rename(hdr.1=ID) %>%
  right_join(lf.dat, by="hdr.1") %>%
  dplyr::select(-hdr.1)

lf.dat3 = evt.in@data@attributes[[1]] %>%
  dplyr::select(ID,EVT_GP_N,EVT_PHYS) %>%
  rename(hdr.2=ID) %>%
  right_join(lf.dat2, by="hdr.2") %>%
  dplyr::select(-hdr.2,-x,-y)

# sum up by categories, calc proportion of landscape
tot.con = length(lf.dat3[lf.dat3$EVT_PHYS=="Conifer",1])
tot.pico = length(lf.dat3[lf.dat3$EVT_GP_N=="Lodgepole Pine Forest and Woodland",1])

lf.conifer = lf.dat3 %>%
  filter(EVT_PHYS=="Conifer") %>%
  mutate(count=1) %>%
  group_by(FBFM40) %>%
  summarise(prop=sum(count)/tot.con)

levels(lf.dat3$EVT_GP_N)

lf.pico = lf.dat3 %>%
  filter(EVT_GP_N=="Lodgepole Pine Forest and Woodland") %>%
  mutate(count=1) %>%
  group_by(FBFM40) %>%
  summarise(prop=sum(count)/tot.pico)

# 
# lf.dat3 %>%
#   filter(EVT_PHYS %in% c("Shrubland","Conifer","Hardwood","Conifer-Hardwood")) %>%
#   group_by(FBFM40) %>%
#   tally() 
#   ggplot(aes(x=FBFM40, y=n)) +
#   geom_bar(stat="identity")
# 
# unique(lf.dat3$EVT_PHYS)
# 
# lf.conifer %>%
#   ggplot(aes(x=FBFM40, y=prop)) +
#   geom_bar(stat="identity")


write.csv(lf.conifer,"outputs/analysis/fbfm_ros_runs/conifer_fbfms.csv", row.names=FALSE)
write.csv(lf.pico,"outputs/analysis/fbfm_ros_runs/pico_fbfms.csv", row.names=FALSE)


#####
# 3. loading in and selecting FBFMs, fuel moisture, wind scenarios
#####

### FBFMs
data("SFM_metric")

SFM_metric$fbfm = rownames(SFM_metric)
  
# reviewed FBFMs in LANDFIRE v2 (remap). For simplicitly, I am starting with a subset of the classes from these studies and narrowing further to only timber understory (semiarid to subhumid climate) and timber litter (non-broadleaf) models
sfm.list = c("TU1","TU5","TL3","TL4","TL5")

# write out metric values for fuels in chosen fbfms
SFM_metric %>%
  filter(fbfm %in% sfm.list) %>%
  write.csv("outputs/analysis/fbfm_ros_runs/fbfm_fuels.csv", row.names=FALSE)

### fuel moisture scenarios
data("scenarios")

# select scenarios where D and L moisture levels match
fm.list = c("D1L1","D2L2","D3L3","D4L4")

### wind scenarios
# all values between 15 and 60 by 5 km/hr increments
# 3 potential WAFs
wind = data.frame(open_wind = rep(seq(15,60,5),3), waf = c(rep(0.1,10),rep(0.2,10),rep(0.3,10))) %>%
  mutate(midflame_wind = open_wind * waf)

# also using same open wind speeds, but fixed WAF of 0.4 for crown fire spread
# Scott & Reinhardt 2001
wind_crown = data.frame(open_wind = rep(seq(15,60,5)), waf = 0.4) %>%
  mutate(midflame_wind = open_wind * waf)


### fuel load scenarios

fuels.in = expand.grid(ffuel = seq(2.5,25,2.5), cfuel=seq(0,15,2.5), regen = seq(0,22,1)) 


#####
# 4. calculate ROS for FBFMs and scenarios
#####

# default fbfm fuels
ros.out=data.frame()

for(i in 1:length(sfm.list)) {
  
  for(j in 1:length(fm.list)) {
    
    for(k in 1:length(wind[,1])) {
        
        modeltype = SFM_metric[sfm.list[i], "Fuel_Model_Type"]
        w = SFM_metric[sfm.list[i],2:6]
        s = SFM_metric[sfm.list[i], 7:11]
        delta = SFM_metric[sfm.list[i], "Fuel_Bed_Depth"]
        mx.dead = SFM_metric[sfm.list[i], "Mx_dead"]
        h = SFM_metric["TU1", 14:18]
        m = scenarios[fm.list[j], 1:5]
        u = wind$midflame_wind[k]
        slope = 0 
        
        a = ros(modeltype, w, s, delta, mx.dead, h, m, u, slope)
        
        ros.out = rbind(ros.out, as.data.frame(cbind(fbfm = sfm.list[i], open_wind = wind$open_wind[k],
                                                     waf = wind$waf[k], midflame_wind = wind$midflame_wind[k],
                                                     fuel_moisture = fm.list[j],
                                                     one_hr = w[1],
                                                     ten_hr = w[2],
                                                     hundred_hr = w[3],
                                                     woody = w[5],
                                                     sav = a[[4]],
                                                     ir = a[[10]],
                                                     ros = a[[15]])))
        
    }
  } 
}


# modified fuel loads

ros.out2=data.frame()

for(i in 1:length(sfm.list)) {
  
  fuel_props = SFM_metric[sfm.list[i],] %>%
    mutate(one_hr_prop = Load_1h/(Load_1h + Load_10h), 
           ten_hr_prop = Load_10h/(Load_1h + Load_10h))
  
  
  for(j in 1:length(fm.list)) {
    
    for(k in 1:length(wind[,1])) {
      
      print(paste0("working on FBFM ", sfm.list[i],", moisture ",fm.list[j],", wind ",wind$midflame_wind[k]))
      
      for(h in 1:length(fuels.in[,1])) {
      
        # allocate fuels according to fuel model proportions
        fuels = fuels.in %>%
          mutate(one_hr = ffuel * fuel_props$one_hr_prop, ten_hr = ffuel * fuel_props$ten_hr_prop,
                 hundred_hr = cfuel, herb=0, woody=regen)
        
        # skip regen runs based on fuel model type
        if(fuels$regen[h]==0 & sfm.list[i] %in% c("TU1","TU5")) next
        if(fuels$regen[h]>0 & sfm.list[i] %in% c("TL1","TL3","TL4","TL5")) next
        
      
      modeltype = SFM_metric[sfm.list[i], "Fuel_Model_Type"]
      w = fuels[h,4:8]
      s = SFM_metric[sfm.list[i], 7:11]
      delta = SFM_metric[sfm.list[i], "Fuel_Bed_Depth"]
      mx.dead = SFM_metric[sfm.list[i], "Mx_dead"]
      h = SFM_metric["TU1", 14:18]
      m = scenarios[fm.list[j], 1:5]
      u = wind$midflame_wind[k]
      slope = 0 
      
      a = ros(modeltype, w, s, delta, mx.dead, h, m, u, slope)
      
      ros.out2 = rbind(ros.out2, as.data.frame(cbind(fbfm = sfm.list[i], open_wind = wind$open_wind[k],
                                                   waf = wind$waf[k], midflame_wind = wind$midflame_wind[k],
                                                   fuel_moisture = fm.list[j],
                                                   one_hr = w[1],
                                                   ten_hr = w[2],
                                                   hundred_hr = w[3],
                                                   woody = w[5],
                                                   sav = a[[4]],
                                                   ir = a[[10]],
                                                   ros = a[[15]])))

      }
    }  
  }
}


# for Anderson's FM10, crown fire spread
ros.out3=data.frame()

for(j in 1:length(fm.list)) {
  
  for(k in 1:length(wind_crown[,1])) {
    
    modeltype = SFM_metric["A10", "Fuel_Model_Type"]
    w = SFM_metric["A10",2:6]
    s = SFM_metric["A10", 7:11]
    delta = SFM_metric["A10", "Fuel_Bed_Depth"]
    mx.dead = SFM_metric["A10", "Mx_dead"]
    h = SFM_metric["A10", 14:18]
    m = scenarios[fm.list[j], 1:5]
    u = wind_crown$midflame_wind[k]
    slope = 0 
    
    a = ros(modeltype, w, s, delta, mx.dead, h, m, u, slope)
    
    ros.out3 = rbind(ros.out3, as.data.frame(cbind(fbfm = "A10", open_wind = wind_crown$open_wind[k],
                                                 waf = wind_crown$waf[k], midflame_wind = wind_crown$midflame_wind[k],
                                                 fuel_moisture = fm.list[j],
                                                 one_hr = w[1],
                                                 ten_hr = w[2],
                                                 hundred_hr = w[3],
                                                 woody = w[5],
                                                 sav = a[[4]],
                                                 ir = a[[10]],
                                                 ros = a[[15]])))
    
  }
} 



#####
# 5. calculate intensity and flame length
#####

# intensity from andrews 2018
# modifications to eq for units (m2/m3 to cm2/cm3; kW/m2 to kJ/min/m2)
ros.out$ib = ros.out$ir * ros.out$ros * 12.6/(ros.out$sav/100)
ros.out2$ib = ros.out2$ir * ros.out2$ros * 12.6/(ros.out2$sav/100)
ros.out3$ib = ros.out3$ir * ros.out3$ros * 12.6/(ros.out3$sav/100)


# flame lenth from byram 1959, alexander 1982 for surface fire
ros.out$surface_fl = 0.0775 * ros.out$ib ^ 0.46
ros.out2$surface_fl = 0.0775 * ros.out2$ib ^ 0.46
ros.out3$ib = ros.out3$ir * ros.out3$ros * 12.6/(ros.out3$sav/100)



#####
# 6. output data
#####

write.csv(ros.out, file="outputs/analysis/fbfm_ros_runs/ros_default.csv", row.names = FALSE)
write.csv(ros.out2, file="outputs/analysis/fbfm_ros_runs/ros.csv", row.names = FALSE)
write.csv(ros.out3, file="outputs/analysis/fbfm_ros_runs/ros_crown.csv", row.names = FALSE)

# also save in iland output processing
write.csv(ros.out2, file="scripts/iland_output_processing/ros.csv", row.names = FALSE)
write.csv(ros.out3, file="scripts/iland_output_processing/ros_crown.csv", row.names = FALSE)

#####
# 7. generate simple plots for comparisons and relationships
#####

pal = brewer.pal(n=5,"YlGn")[c(2:5)]

# default ros and flame lenths in English units, to show comparison with Scott & Burgan paper
def1 = ros.out %>% ggplot(aes(x=(midflame_wind*0.621371), y=(ros*2.98258), col=factor(fuel_moisture))) +
  facet_wrap(~fbfm) +
  geom_line(size=1) +
  ylab("Rate of spread (ch/h)") +
  xlab("Midflame wind speed (mi/h)") +
  scale_color_manual(name="Fuel moisture",
                       labels=c("Very low","Low","Moderate","High"),
                       values=pal) +
  theme_bw() 


def2 = ros.out %>% ggplot(aes(x=(midflame_wind*0.621371), y=(surface_fl*3.28084), col=factor(fuel_moisture))) +
  facet_wrap(~fbfm) +
  geom_line(size=1) +
  ylab("Flame length (ft)") +
  xlab("Midflame wind speed (mi/h)") +
  scale_color_manual(name="Fuel moisture",
                     labels=c("Very low","Low","Moderate","High"),
                     values=pal) +
  theme_bw() 

pdf("outputs/figures/fbfm_ros/default_ros_flame_length_english.pdf", height=5.5, width=6.5)
def1
def2
dev.off()

# default ros and flame lenths in metric units
def3 = ros.out %>% ggplot(aes(x=(midflame_wind), y=(ros), col=factor(fuel_moisture))) +
  facet_wrap(~fbfm) +
  geom_line(size=1) +
  ylab("Rate of spread (m/min)") +
  xlab("Midflame wind speed (km/h)") +
  scale_color_manual(name="Fuel moisture",
                     labels=c("Very low","Low","Moderate","High"),
                     values=pal) +
  theme_bw() 


def4 = ros.out %>% ggplot(aes(x=(midflame_wind), y=(surface_fl), col=factor(fuel_moisture))) +
  facet_wrap(~fbfm) +
  geom_line(size=1) +
  ylab("Flame length (m)") +
  xlab("Midflame wind speed (km/h)") +
  scale_color_manual(name="Fuel moisture",
                     labels=c("Very low","Low","Moderate","High"),
                     values=pal) +
  theme_bw() 

pdf("outputs/figures/fbfm_ros/default_ros_flame_length_metric.pdf", height=5.5, width=6.5)
def3
def4
dev.off()

# comparison with altered fuel loads
# now in metric

labels = as_labeller(c(
  "D1L1" = "Very low",
  "D2L2" = "Low",
  "D3L3" = "Moderate",
  "D4L4" = "High",
  "TU1" = "TU1", "TU5" = "TU5", "TL3" = "TL3", "TL4" = "TL4", "TL5" = "TL5"
))

ros.out2 %>%
  ggplot(aes(x=midflame_wind, y=surface_fl)) +
  facet_wrap(fbfm~fuel_moisture, labeller=labels, ncol=4) +
  geom_point() +
  geom_line(aes(x=midflame_wind, y=surface_fl), data=ros.out, col="red") +
  xlab("Midflame wind speed (km/h)") +
  ylab("Flame length (m)") +
  theme_bw() +
  ggsave(filename = "outputs/figures/fbfm_ros/flame_length_comparison.pdf",width=6.5, height=9)

ros.out2 %>%
  ggplot(aes(x=midflame_wind, y=ib)) +
  facet_wrap(fbfm~fuel_moisture, labeller=labels, ncol=4) +
  geom_point() +
  geom_line(aes(x=midflame_wind, y=ib), data=ros.out, col="red") +
  xlab("Midflame wind speed (km/h)") +
  ylab("Intensity (kW/m)") +
  theme_bw() +
  ggsave("outputs/figures/fbfm_ros/intensity_comparison.pdf",width=6.5, height=9)


# effect of fuel load on intensity and flame lengthn at constant wind and moisture

i1 = ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(one_hr+ten_hr), y=ib)) +
  ylab("Intensity (kW/m)") +
  xlab("Available fine fuels (1h + 10h) (Mg/ha)") +
  geom_point() +
  theme_bw()

i2= ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(hundred_hr), y=ib)) +
  ylab("Intensity (kW/m)") +
  xlab("Available coarse fuels (100h) (Mg/ha)") +
  geom_point() +
  theme_bw()

i3 = ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(woody), y=ib)) +
  ylab("Intensity (kW/m)") +
  xlab("Live woody fuels (Mg/ha)") +
  geom_point() +
  theme_bw()

ggsave("outputs/figures/fbfm_ros/tu5_12kmh_verydry_intensity_fuels.pdf", plot_grid(i1,i2,i3, nrow=2, align="hv"), width=6.5, height=5.5)

fl1 = ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(one_hr+ten_hr), y=surface_fl)) +
  ylab("Flame length (m)") +
  xlab("Available fine fuels (1h + 10h) (Mg/ha)") +
  geom_point() +
  theme_bw()

fl2= ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(hundred_hr), y=surface_fl)) +
  ylab("Flame length (m)") +
  xlab("Available coarse fuels (100h) (Mg/ha)") +
  geom_point() +
  theme_bw()

fl3 = ros.out2 %>% 
  filter(fbfm == "TU5" & midflame_wind == 12 & fuel_moisture =="D1L1") %>%
  ggplot(aes(x=(woody), y=surface_fl)) +
  ylab("Flame length (m)") +
  xlab("Live woody fuels (Mg/ha)") +
  geom_point() +
  theme_bw()

ggsave("outputs/figures/fbfm_ros/tu5_12kmh_verydry_flame_length_fuels.pdf", plot_grid(fl1,fl2,fl3, nrow=2, align="hv"), width=6.5, height=5.5)
