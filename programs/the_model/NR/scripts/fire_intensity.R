#####
#
## fire intensity estimation using iland outputs
#
#####

rm(list=ls())

# libraries
library(raster)
library(dplyr)
library(dbplyr)
library(tidyr)
library(RSQLite)

# versioning and variable setup

filename = list.files("NR/output/", pattern="*.sqlite")

filetable = strsplit(sub('\\.sqlite$', '', filename), "_")[[1]]
forest_type = filetable[1]
clim = filetable[2]
scen = filetable[3]
prop = filetable[4]
rep = filetable[6]


start_year = 1979
end_year = start_year + 120
buffer_width = 2300  # (15000 - 10400)/2
xmin = 0
xmax = 15000 - (2 * buffer_width)
ymin = 0
ymax = 15000 - (2 * buffer_width)

# load rids for study region
# this is just the core 100 x 100 ha
env.rids = read.table("NR/gis/analysis/env_rids.txt")[,1]


#####
# 1. loading in iland stand-level outputs and ros scenarios
#####

stand.in = read.csv(paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_stand_",rep,".csv")) %>%
  filter(year<end_year)  # remove final year, because this would be pre-fire conditions for following year

ros.scenarios = read.csv("NR/scripts/ros/ros.csv")
# change factors to character vectors
ros.scenarios$fbfm = as.character(ros.scenarios$fbfm)
ros.scenarios$fuel_moisture = as.character(ros.scenarios$fuel_moisture)

ros.crown = read.csv("NR/scripts/ros/ros_crown.csv") %>%
  mutate(ros_crown = ros * 3.34)  # per Rothermel, Scott 

ros.crown$fuel_moisture = as.character(ros.crown$fuel_moisture)

#####
# 2. assign fbfms
#####

# tu vs tl first, based on threshold for live biomass
# in regeneration layer
stand.in$fbfm.type = ifelse(stand.in$woody_fuel*0.001>=0.5, "TU","TL")


# then tu subdivided based on threshold for 1h + 10h fuel load (low, high)
# then tl subdivided first based on thresholds for 1h + 10h fuel load (low, high)
# moderate litter fuel load further subdivided by presence of larger fuels (100h + 1000h fuels)
# into low and high

stand.in$fbfm = ifelse(
  stand.in$fbfm.type =="TU" & stand.in$fine_fuel*0.001 < 5, "TU1",
  ifelse(
    stand.in$fbfm.type == "TU" & stand.in$fine_fuel*0.001 >= 5, "TU5",
    ifelse(
      stand.in$fbfm.type =="TL" & stand.in$fine_fuel*0.001 >= 17.5, "TL5",
      ifelse(
        stand.in$fbfm.type=="TL" & 
          stand.in$fine_fuel*0.001 < 17.5 &
          stand.in$coarse_fuel*0.001/2 < 7.5,"TL3","TL4"))))
        

# # write out FBFM distributions
# # if needed, can derive these from stand outputs
# stand.in %>%
#   mutate(count=1, pico=ifelse(pico_iv>=1.5,"pico","non-pico")) %>%
#   group_by(fbfm,pico) %>%
#   summarise(count=sum(count)) %>%
#   write.csv(paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_fbfms_",rep,".csv"),row.names=FALSE)

#####
# 3. assign wind, fuel moisture, and fuel load scenarios to ha that burned
#####

### fuel moisture
# based on kbdi anomaly, which varies by climate scenario

# lookup table
kbdi.in = data.frame(clim = c("CanESM2","HadGEM2CC","HadGEM2ES"), kbdi_ref = c(0.208,0.202,0.200))
kbdi.ref = kbdi.in[kbdi.in$clim == clim,]$kbdi_ref

# add fuel moisture scenario
stand.in$fuel_moisture = ifelse(
  stand.in$fire_kbdi/kbdi.ref < 1, "D4L4",
  ifelse(
    stand.in$fire_kbdi/kbdi.ref >= 1 & stand.in$fire_kbdi/kbdi.ref < 1.35, "D3L3",
    ifelse(
      stand.in$fire_kbdi/kbdi.ref >= 1.35 & stand.in$fire_kbdi/kbdi.ref < 1.7, "D2L2", "D1L1"
    )
  )
)


### wind
# adapt windspeed from iland to gust scale from RAWS

stand.in$raws_windspeed = ifelse(stand.in$fire_windspeed>0,
                                 (45/20) * stand.in$fire_windspeed - 7.5,0)

# round to nearest 5 km/hr
stand.in$open_wind = 5*round(stand.in$raws_windspeed/5)

# determine which waf to use, unsheltered, sheltered open, or sheltered dense
# based on anderson 2012, crown ratio and canopy cover
stand.in$f = stand.in$cr * stand.in$stocked_area/3
stand.in$waf = ifelse(stand.in$f < 0.05, 0.3,
                      ifelse(stand.in$f >= 0.05 & stand.in$f < 0.15, 0.2, 0.1))

### fuel load
# add total fine fuel to ros scenarios
# fine fuel every 2.5
# coarse fuel (hundred hour only) every 2.5
ros.scenarios$ff_round = round(ros.scenarios$one_hr + ros.scenarios$ten_hr,1)

# calculate rounded values for stands
# set 2.5 as minimum value to enable intensity calculations in event fire spread occurs
stand.in$ff_round = ifelse((round((stand.in$fine_fuel * 0.001)/2.5) * 2.5)>0,
                           round((stand.in$fine_fuel * 0.001)/2.5) * 2.5, 2.5)  # to nearest 2.5

stand.in$hundred_hr = round((stand.in$coarse_fuel * 0.001 * 0.5)/2.5) * 2.5

stand.in$woody = round(stand.in$woody_fuel*0.001)

# check max fuels and assign max value if exceeded
if(max(stand.in$ff_round>25)) {
  stand.in$ff_round = ifelse(stand.if$ff_round <= 25, stand.in$ff_round, 25)
  write.csv(unique(stand.in$ff_round), file=paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_finefuels_",rep,".csv"), row.names=FALSE)
}

if(max(stand.in$hundred_hr>15)) {
  stand.in$hundred_hr = ifelse(stand.in$hundred_hr <=15, stand.in$hundred_hr, 15)
  write.csv(unique(stand.in$hundred_hr), file=paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_hundredhr_",rep,".csv"), row.names=FALSE)
}

if(max(stand.in$woody>22)) {
  stand.in$woody = ifelse(stand.in$woody <=22, stand.in$woody, 22)
  write.csv(unique(stand.in$woody), file=paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_woody_",rep,".csv"), row.names=FALSE)
}

### match up appropriate ros scenario based on wind, fuel moisture, fuel load
# select only needed columns
# subset to areas that burned based on fire spread >= 0.5
stand.fire = stand.in %>%
  dplyr::select(-c(pico_iv:stocked_area),-fbfm.type,-raws_windspeed,-f) %>%
  filter(fire_spread>=0.5) %>%
  left_join(ros.scenarios, by=c("fbfm","open_wind","waf","fuel_moisture","ff_round","hundred_hr","woody")) %>%
  left_join(ros.crown[,c("open_wind","fuel_moisture","ros_crown")], by=c("open_wind","fuel_moisture")) %>%
  rename(cros=ros_crown)

#####
# 4. calculate crown fire initiation, initial fire classification
#####

### add foliar moisture content and 1hr moisture content
stand.fire$fmc = ifelse(
  stand.fire$fuel_moisture == "D1L1", 70,
  ifelse(
    stand.fire$fuel_moisture == "D2L2", 90,
    ifelse(
      stand.fire$fuel_moisture == "D3L3", 120, 150
    )
  )
)

stand.fire$onehr = ifelse(
  stand.fire$fuel_moisture == "D1L1", 3,
  ifelse(
    stand.fire$fuel_moisture == "D2L2", 6,
    ifelse(
      stand.fire$fuel_moisture == "D3L3", 9, 12
    )
  )
)


### calculate crown fire thresholds

# crown fire initiation
stand.fire$io = (0.010 * stand.fire$cbh * (460 + 25.9 * stand.fire$fmc)) ^ 1.5

# criteria for active crowning
stand.fire$cac = stand.fire$cros / (3/stand.fire$cbd)

# second conditional test per Scott & Reinhardt, would fire drop down if active crown fire adjacent
stand.fire$ib_cros = stand.fire$ir * stand.fire$cros * 12.6/(stand.fire$sav/100)


### classify fires
# partly from decision tree from Nelson
# with additional criteria, not conditional crown if would "drop down" per Scott & Reinhardt
# needs to have canopy fuel present to be passive crown fire

stand.fire$surf_to_crown = ifelse(stand.fire$ib >= stand.fire$io & stand.fire$cfl > 0, "true", "false")

stand.fire$crown_spread = ifelse(stand.fire$cac >= 1 & stand.fire$ib_cros >= stand.fire$io, "true","false")


stand.fire$class = ifelse(
  stand.fire$surf_to_crown =="true",
  ifelse(stand.fire$crown_spread=="true", "active_crown", "passive_crown"),
  ifelse(stand.fire$crown_spread=="true", "conditional_crown", "surface")
)


#####
# 5. spread crown fire
#####

# assign integer value to classes
stand.fire$class_int = ifelse(stand.fire$class == "surface",1,
                              ifelse(stand.fire$class=="passive_crown",2,
                                     ifelse(stand.fire$class=="conditional_crown",3,
                                            ifelse(stand.fire$class=="active_crown",4,0))))


# get list of all rids from stand.in
rid.list = stand.in %>%
  filter(year==start_year+1) %>%
  dplyr::select(rid,x,y)

# udpdate x and y coordinates to reflect omission of buffer
rid.list$x_core = rid.list$x - buffer_width
rid.list$y_core = rid.list$y - buffer_width

new.classes = data.frame()

# iterate through each year and each cell up to 100 interations
# to spread active crown fire in all possible directions
# if cell is conditional crown fire adjacent to active crown fire,
# cell will be reclassified as active crown fire and updated with each
# iteration until no more spread is possible or 100 iterations are reached
for(k in min(stand.fire$year):max(stand.fire$year)) {
  
  value="class_int"
  
  df = stand.fire %>%
    filter(year==k) %>%
    dplyr::select(rid,value) %>%
    right_join(rid.list, by="rid") %>%
    dplyr::select(-x,-y)
  
  df[is.na(df)] = 0
  
  sp1 = with(df,SpatialPointsDataFrame(coords=cbind(x_core,y_core),data.frame(df[,value])))
  r1 = raster(xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax, nrows=(ymax-ymin)/100, ncols=(xmax-xmin)/100)  
  r.out = rasterize(sp1,r1,df[,value])
  
  r.new=r.out
  
  for(j in 1:100) {
    continue="no"
    r.out=r.new
    for(i in 1:ncell(r.out)) {
      df.sub = df[i,]
      cell = cellFromXY(r.out,c(df.sub$x_core,df.sub$y_core))
      cell.value = r.out[cell]
      if(cell.value == 3) {
        cell.adj = adjacent(r.out,cell,8)
        cell.value2 = r.out[c(cell.adj[,2])]
        cell.value3 = ifelse(cell.value2 ==4, 1,0)
        cspread = sum(cell.value3)/length(cell.value3)
        # print(cspread)
        if(cspread > 0) {  # if(cspread >=0.5) {
          r.new[cell] = 4
          continue="yes"
        }
      }
    }
    if(continue=="no") {break}
  }
  
  new.classes = rbind(new.classes,
                      as.data.frame(cbind(
                        df,
                        year = k,
                        new_class_int = raster::extract(r.new, df[,c("x_core","y_core")]))))
  
}


stand.out = stand.fire %>%
  left_join(new.classes, by=c("rid","year","class_int"))


stand.out$new_class = ifelse(stand.out$new_class_int %in% c(1,3),"surface",
                                 ifelse(stand.out$new_class_int==2,"passive_crown","active_crown"))

#####
# 6. calculate final ROS
#####


### calculate final ROS

# assume crown fraction burned is 0 for surface and conditional, 0.5 for passive, 1 for active)
stand.out$cfb = ifelse(
  stand.out$new_class == "surface", 0,
  ifelse(
    stand.out$new_class == "passive_crown", stand.out$cac, 1)
)

# final ros
stand.out$ros_final = stand.out$ros + stand.out$cfb * (stand.out$cros - stand.out$ros)


### calc final intensity

h.crown = 18000

# Scott & Reinhdardt
stand.out$i_final = ((stand.out$ib/stand.out$ros) * 60  + (stand.out$cfl * h.crown * stand.out$cfb)) * (stand.out$ros_final/60)


### assign fireline intensity class
# flame length

stand.out$flame_length = ifelse(
  stand.out$new_class == "surface",
  0.0775 * stand.out$i_final ^ 0.46,
  0.02665 * stand.out$i_final ^ 0.67)


stand.out$fil = ifelse(
  stand.out$flame_length < 0.6, 1,
  ifelse(
    stand.out$flame_length >=0.6 & stand.out$flame_length < 1.2, 2,
    ifelse(
      stand.out$flame_length >=1.2 & stand.out$flame_length < 1.8, 3,
      ifelse(
        stand.out$flame_length >=1.8 & stand.out$flame_length < 2.4, 4,
        ifelse(
          stand.out$flame_length >= 2.4 & stand.out$flame_length < 3.7, 5,
          ifelse(
            stand.out$flame_length >=3.7 & stand.out$flame_length < 15, 6,7
          ))))))


stand.out$intensity_class = ifelse(stand.out$fil>=5,"high",
                                       ifelse(stand.out$fil>=3,"moderate","low"))

#####
# 7. summary landscape level data
#####

# get annual kbdi for study area, all stands
kbdi.ann = stand.in %>%
  filter(rid %in% env.rids) %>% # subset to study area
  dplyr::select(fire_year,fire_kbdi) %>%
  group_by(fire_year) %>%
  summarise_all(mean)
  

# classify
stand.class = stand.out %>%
  dplyr::select(fire_year,rid,new_class,intensity_class) %>%
  filter(rid %in% env.rids) %>% # subset to study area
  group_by(fire_year,rid) %>%
  mutate(surface=ifelse(new_class=="surface",1,0),
         passive_crown=ifelse(new_class=="passive_crown",1,0),
         active_crown=ifelse(new_class=="active_crown",1,0),
         crown=ifelse(new_class %in% c("passive_crown","active_crown"),1,0),
         low=ifelse(intensity_class=="low",1,0),
         moderate=ifelse(intensity_class=="moderate",1,0),
         high=ifelse(intensity_class=="high",1,0),
         burned=ifelse(intensity_class %in% c("low","moderate","high"),1,0))

# aggregate to landscape
# and add annual kbdi
land.out = stand.class %>%
  group_by(fire_year) %>%
  dplyr::select(fire_year,surface:burned) %>%
  summarise_all(sum) %>%
  full_join(kbdi.ann, by="fire_year") %>%
  gather("variable","value",-fire_year)

# replace na values with 0
land.out[is.na(land.out)] = 0


#####
# 8. write out data
#####

write.csv(stand.out, file=paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_fireintensity_",rep,".csv"), row.names=FALSE)
write.csv(land.out, file=paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_landscapefire_",rep,".csv"), row.names=FALSE)

print(paste0(filename," fire intensity complete"))
