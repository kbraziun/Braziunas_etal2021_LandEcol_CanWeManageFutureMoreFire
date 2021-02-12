#####
#
## create generic forest development outputs csv
## for over time plots and for fire intensity calcs
## trees are considered as > 2m in height
#
#####

# this file is meant to be called from within a bash script
# and it will live in the NR/scripts/ folder

rm(list=ls())

# wd inherited from project

library(raster)
library(dplyr)
library(dbplyr)
library(RSQLite)
library(tidyr)

#####
# 1. setup variables and load rids
#####

# no command line arguments
# but could add in future
# args = commandArgs(TRUE)
# start_year = as.integer(args[1])
start_year = 1979

filename = list.files("NR/output/", pattern="*.sqlite")

filetable = strsplit(sub('\\.sqlite$', '', filename), "_")[[1]]
forest_type = filetable[1]
clim = filetable[2]
scen = filetable[3]
prop = filetable[4]
rep = filetable[6]

# load rids for study region
# this is just the core 100 x 100 ha
env.rids = read.table("NR/gis/analysis/env_rids.txt")[,1]

# load rids for given scenario
if (scen %in% c("random","cluster")) {
  
  house.rids = read.csv(paste0("NR/gis/mgmt/",scen,"_",prop,"_",rep,".csv"), header=TRUE)[,1]

} else if (scen %in% "nomgmt") {
  
  house.rids = 0
  
}

#####
# 2. loading in iland stand-level outputs
#####

conn=DBI::dbConnect(RSQLite::SQLite(), dbname = paste0("NR/output/",filename)) # connect to the db
dbListTables(conn)


# aggregate across all species in stand
# dominant height is the density-weighted avg of the 90th pctile height across all species
# stand age is the mean age of all trees >4m ht (calc'ed here also with density weighting)
stand.in = tbl(conn, "dynamicstand") %>%
  dplyr::select(-ru,-species) %>%
  rename(count_4m=if_height_3_1_0_sum, basalarea_4m=basalarea_sum) %>%
  mutate(height_p90 = height_p90 * count_4m, age_p90 = age_p90 * count_4m,
         year = year + start_year, height_mean = height_mean * count_4m) %>%  # step 1 in calc overall weighted mean for stand, multiply species-specific mean x species-specific density
  filter(year>start_year) %>% # remove year 0, no carbon outputs for this year
  group_by(year,rid) %>%
  summarise_all(sum,na.rm=TRUE) %>%  # summing across all species, all metrics
  mutate(height_p90 = height_p90/count_4m, age_p90 = age_p90/count_4m,
         height_mean = height_mean/count_4m) %>%  # step 2 in calc overall mean, dividing sum by total tree density
  rename(height_mean_4m=height_mean, age_p90_4m=age_p90) %>%
  collect()


# also read in species-level values
# combine pico and pics
# just need count and basal area
stand.specin = tbl(conn, "dynamicstand") %>% 
  dplyr::select(-ru,-height_p90,-age_p90, -height_mean) %>%
  rename(count_4m_sp=if_height_3_1_0_sum, basalarea_4m_sp=basalarea_sum) %>%
  mutate(year = year+start_year, 
         species = ifelse(species=="PicS", "Pico", species)) %>%  # assign Pico to PicS for aggregation
  filter(year > start_year) %>%
  group_by(year,rid,species) %>%
  summarise_all(sum, na.rm=TRUE) %>%
  collect()


# aggregate across sapling layer canopy fuels in stand
# only including saplings > 2m
# have to calculate basal area, derive from DBH, mult by stem count of all cohorts
# also calc'ing mean age for cohorts > BH (with density weighting)
sap.in = tbl(conn, "saplingdetail") %>%
  filter(height >=2) %>%
  dplyr::select(year,rid,n_represented,dbh, height, age) %>%
  mutate(basalarea_2m = ((pi * ((dbh/200) ^ 2)) * n_represented), 
         ht_sap_sum = height * n_represented, age_sap_sum = age * n_represented,
         year = year + start_year) %>%  # add ba, get sum height for later calculations, step 1 in weighted avg
  filter(year>start_year) %>%
  group_by(year,rid) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  mutate(height_mean_2m = ht_sap_sum/n_represented, age_mean_2m = age_sap_sum/n_represented) %>%
  rename(count_2m=n_represented) %>%
  dplyr::select(-dbh,-height,-ht_sap_sum,-age,-age_sap_sum) %>%
  collect()


# also sapling detail by species
# just need count and basal area
sap.specin = tbl(conn, "saplingdetail") %>%
  filter(height >=2) %>%
  dplyr::select(year,rid,species,n_represented,dbh) %>%
  mutate(basalarea_2m_sp = ((pi * ((dbh/200) ^ 2)) * n_represented), 
         year = year + start_year,
         species = ifelse(species=="PicS", "Pico", species)) %>%  # summarize Pico as single species
  filter(year>start_year) %>%
  group_by(year,rid,species) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  rename(count_2m_sp=n_represented) %>%
  dplyr::select(-dbh) %>%
  collect()


# also bring in sapling table for number of stems in sapling layer
sapcount.in = tbl(conn, "sapling") %>%
  dplyr::select(year,rid,count_ha,count_small_ha) %>%
  mutate(year = year + start_year,
         count_below_4m = count_ha + count_small_ha) %>%
  filter(year>start_year) %>%
  group_by(year,rid) %>%
  dplyr::select(-count_ha,-count_small_ha) %>%
  summarise_all(sum,na.rm=TRUE) %>%
  collect()


# read in carbon values used to calculate fuels
# also get aboveground live and dead carbon pools
carbon.in = tbl(conn, "carbon") %>%
  filter(rid > 1) %>%
  dplyr::select(year,rid,stem_c,branch_c,foliage_c,regeneration_c,snags_c,snagsOther_c_ag,downedWood_c_ag,litter_c_ag) %>%
  mutate(year = year + start_year) %>%
  filter(year>start_year) %>%
  group_by(year,rid) %>%
  mutate(live_c_ag = stem_c + branch_c + foliage_c + 0.8 * regeneration_c,
         dead_c_ag = snags_c + snagsOther_c_ag + downedWood_c_ag + litter_c_ag) %>%  # add up aboveground carbon pools, estimate 80% of regen C is aboveground per iland reburns analysis 
  dplyr::select(year,rid,live_c_ag,dead_c_ag,regeneration_c,downedWood_c_ag,litter_c_ag) %>%
  collect()

# read in water outputs for stocked area
water.in = tbl(conn, "water") %>%
  filter(rid > 1) %>%
  dplyr::select(year,rid,stocked_area) %>%
  mutate(year = year + start_year) %>%
  filter(year>start_year) %>%
  collect()


# read in fire values used to calculate fuels
# also for information on annual ha burned
fire.in = tbl(conn, "fire") %>%
  dplyr::select(year,fireId,area_m2,avgFuel_kg_ha, windSpeed) %>%
  mutate(year = year + start_year) %>%
  collect()
  
dbDisconnect(conn) # close the connection

#####
# 3. calculate variables of interest
#####


# build template for stand level outputs
end_year = max(stand.in$year)
min_rid = min(stand.in$rid)
max_rid = max(stand.in$rid)

stand.temp = expand.grid(rid = seq(min_rid,max_rid,by=1),year = seq(start_year+1,end_year,by=1))

# combined outputs
# first full join at stand level, all species with template
# allows assigning 0 values to stands where no species present
stand.join = stand.temp %>%
  full_join(stand.in, by=c("year","rid")) %>%
  full_join(sap.in, by=c("year","rid")) %>%
  full_join(sapcount.in, by=c("year","rid"))

# remove na values, replace with 0
stand.join[is.na(stand.join)] = 0

# total tree and sapling counts
# weighted averages for tree (> 2m height) variables
# mean stand age is either mean age of trees > 4m or, if no trees > 4m, mean age of
# trees between 2m and 4m
stand.sums = stand.join %>%
  mutate(count_tree = count_4m + count_2m,
         basalarea_tree = basalarea_4m + basalarea_2m,
         height_mean_tree = ((count_4m * height_mean_4m) + (count_2m * height_mean_2m)) / count_tree,
         stand_age = ifelse(age_p90_4m != 0, age_p90_4m, age_mean_2m),
         count_sap = count_below_4m - count_2m) %>%
  dplyr::select(year,rid,count_tree,basalarea_tree,height_p90,height_mean_tree,stand_age,count_sap)

# then join species-level data
stand.full = full_join(stand.specin,sap.specin, by=c("year","rid","species")) %>%
  full_join(stand.sums, by=c("year","rid"))

# remove na values, replace with 0
stand.full[is.na(stand.full)] = 0


# clean up
rm(stand.in,sap.in,stand.specin,sap.specin,sapcount.in,stand.join,stand.temp)


# calc importance value by species as species count/total count + species ba/total ba
# should sum to 2 for the stand
stand = stand.full %>%
  mutate(count_spec = count_4m_sp + count_2m_sp,
         basalarea_spec = basalarea_4m_sp + basalarea_2m_sp,
         count_iv = count_spec / count_tree, basalarea_iv = basalarea_spec / basalarea_tree,
         iv = count_iv + basalarea_iv) 


# classify stands, following wdh methods
# forest is >= 50 stems/ha, stems are for trees > 2m height
# species with highest iv is assigned as dominant (no "mixed" stands)
# young, mid, midold, old assigned by stand age
stand.class = stand %>%
  group_by(year,rid) %>%
  dplyr::select(year,rid,species,height_p90,stand_age,count_tree,basalarea_tree,iv) %>%
  mutate(rank = rank(-iv, ties.method = "first")) %>% # id most dominant species in a given stand 
  filter(rank == 1) %>%
  mutate(forest=ifelse(count_tree>=50,1,0),
         nonforest=ifelse(count_tree<50,1,0),
         species=ifelse(forest==1 & iv >= 1.5,species,
                        ifelse(forest==1,"Mixed","nonforest")),
         pico=ifelse(species=="Pico",1,0),
         abla=ifelse(species=="Abla",1,0),
         pien=ifelse(species=="Pien",1,0),
         psme=ifelse(species=="Psme",1,0),
         potr=ifelse(species=="Potr",1,0),
         mixed=ifelse(species=="Mixed",1,0),
         young=ifelse(forest==1 & stand_age<=40,1,0),
         mid=ifelse(forest==1 & stand_age>40 & stand_age<=100,1,0),
         midold=ifelse(forest==1 & stand_age>100 & stand_age<=250,1,0),
         old=ifelse(forest==1 & stand_age>250,1,0)) 

# aggregate to landscape
landscape = stand.class %>%
  filter(rid %in% env.rids) %>% # filter so only aggregating across study region
  group_by(year) %>%
  dplyr::select(year,forest:old) %>%
  summarise_all(sum) %>%
  gather("variable","value",-year)


# also add to stand output table
# first add pico iv
# then add dominant species
# then add carbon outputs
stand.out = stand %>%
  filter(species=="Pico") %>%
  dplyr::select(year,rid,iv) %>%
  right_join(stand.sums, by=c("year","rid")) %>%
  rename(pico_iv = iv) %>% # pico iv added
  full_join(stand.class[,c("year","rid","species")], by=c("year","rid")) %>%  # dom species added
  full_join(carbon.in, by=c("year","rid")) %>%
  full_join(water.in, by=c("year","rid"))


# change NA species values to nonforest
stand.out$species[is.na(stand.out$species)]= "nonforest"


# remove na values, replace with 0
stand.out[is.na(stand.out)] = 0


#####
# 4. add fire data
#####

### function to read asc files

iland.asclist = function(path, name, firelist, xmn, xmx, ymn, ymx) {
  # create the first raster to stack everything on top of
  r1 = read.table(paste0(path,name,"1.txt"),skip=6)  # read in output from fire 1
  r.stack = raster(as.matrix(r1), xmn, xmx, ymn, ymx)  # create raster, set bounding box
  names(r.stack) = paste0(name,"_1")
  
  # stack the rest of the fire outputs in the same raster
  for(i in 1:length(firelist)) {
    i=firelist[i]
    r = read.table(paste0(path,name,i,".txt"),skip=6)
    rast = raster(as.matrix(r), xmn, xmx, ymn, ymx)
    names(rast) = paste0(name,"_",i)
    r.stack = stack(r.stack,rast)
    tot.fires = i  # also store data on total number of fires
  }
  return(r.stack)
}

### load in rid and fire year rasters

### env raster with rids
env.in = read.table("NR/gis/env_grid.txt", skip=6)

# set landscape boundaries
xmin=0
xmax=15000
ymin=0
ymax=15000


# using corner coords from ASCII/txt file to turn into raster
env = raster(as.matrix(env.in), xmin, xmax, ymin, ymax)  


### read in fire grids for kbdi and fire spread
# combustible fuel can be calculated from carbon outputs
# kbdi is every year
kbdi.in = iland.asclist("NR/output/fire/","kbdi",2:(end_year-start_year),xmin,xmax,ymin,ymax)

# spread is every fire
spread.in = iland.asclist("NR/output/fire/","spread",fire.in$fireId[-1],xmin,xmax,ymin,ymax)

# figure out spread as amount of given ha burned, aggregate from 20m to 100m resolution
values(spread.in)[values(spread.in) > 0] = 1
spread.ha = aggregate(spread.in, fact=5, fun=sum)/25

# simplify spread to code for 1 equals fire occurred in that ha
spread.simp = spread.ha
values(spread.simp)[values(spread.simp) >0] = 1

# get rids of ha affected
rid.rast = spread.simp * env 

### extract data from rasters, convert to dataframe

rid.pts = data.frame(rasterToPoints(env)) %>%
  filter(layer > 1) %>%  # exclude rids < 1
  rename(rid=layer)  # rename to rid

fire.out = data.frame()

# match up fire grids with rids
for(i in 1:dim(fire.in)[1]) {
  fire.out = rbind(fire.out,
                   as.data.frame(cbind(
                     rid.pts,
                     # fireId = fire.in$fireId[i], # not currently used
                     windSpeed = fire.in$windSpeed[i],
                     fire_year = fire.in$year[i],
                     fire_spread = raster::extract(spread.ha[[i]], rid.pts[,c(1:2)])
                   )))
}

kbdi.out = data.frame()

# match up kbdi with rids, kbdi is once per year
for(i in 1:(end_year-start_year)) {
  kbdi.out = rbind(kbdi.out,
                   as.data.frame(cbind(
                     rid.pts,
                     fire_year = start_year+i,
                     fire_kbdi = raster::extract(kbdi.in[[i]], rid.pts[,c(1:2)])
                   )))
}


# we only want one entry per year
# sum spread to track total spread over course of year
# note that fire can burn same pixel multiple times, for a spread of >1
# use maximum windSpeed for a given year
# then add fire kbdi
fire.sum = fire.out %>%
  mutate(spread_simp = ifelse(fire_spread>0,1,0)) %>%  # binary variable for extracting windspeed
  group_by(fire_year,rid) %>%
  summarise(fire_windspeed = max(spread_simp * windSpeed),  # if RID burned multiple times in
            # a given year, use the maximum windspeed from fires that occurred on a given year x rid
            fire_spread = sum(fire_spread)) %>%
  right_join(kbdi.out, by=c("fire_year","rid")) %>%
  filter(fire_year>1980)  # no prefire outputs for 1979 for carbon and thus fuels, so drop fire year 1980

# NAs will be introduced for years in which fire did not occur. replace these with 0s.
fire.sum[is.na(fire.sum)] = 0

# add fire outputs to stand outputs
# note that only have fire outputs on fire years
# will want to update kbdi to export every year, and then will have
# fewer outputs and more complete data
stand.out$fire_year = stand.out$year + 1

stand.fire = stand.out %>%
  full_join(fire.sum, by=c("fire_year","rid"))


#####
# 5. calculate fuel loading
#####

## surface fuels
# calculate combustible fuel in three buckets: fine, coarse, and live woody
stand.fire$fine_fuel = (0.8 + 0.2 * stand.fire$fire_kbdi) * 2*stand.fire$litter_c_ag
stand.fire$coarse_fuel = (0.4 * stand.fire$fire_kbdi * stand.fire$downedWood_c_ag*2)
stand.fire$woody_fuel = 2*stand.fire$regeneration_c


## canopy fuels
# using Pico or Psme if either species IV is >=1.5, otherwise using mixed

# calculate CBH
stand.fire$cbh = ifelse(
  stand.fire$species=="Pico",
  -1.475 + 0.613 * stand.fire$height_mean_tree + 0.043 * stand.fire$basalarea_tree,
  ifelse(
    stand.fire$species=="Psme",
    -1.771 + 0.554 * stand.fire$height_mean_tree + 0.045 * stand.fire$basalarea_tree,
    -1.463 + 0.578 * stand.fire$height_mean_tree + 0.026 * stand.fire$basalarea_tree))

stand.fire$cbh = ifelse(stand.fire$cbh >0, stand.fire$cbh, 0)  # set min as 0

# calculate CBD 
stand.fire$cbd = ifelse(
  stand.fire$species=="Pico",
  exp(-7.852 + 0.349 * log(stand.fire$basalarea_tree) + 0.711 * log(stand.fire$count_tree)),
  ifelse(
    stand.fire$species=="Psme",
    exp(-7.380 + 0.479 * log(stand.fire$basalarea_tree) + 0.625 * log(stand.fire$count_tree)),
    exp(-8.445 + 0.319 * log(stand.fire$basalarea_tree) + 0.859 * log(stand.fire$count_tree))))

# calculate CFL
stand.fire$cfl = ifelse(
  stand.fire$species=="Pico",
  exp(-4.066 + 0.910 * log(stand.fire$basalarea_tree) + 0.130 * log(stand.fire$count_tree)),
  ifelse(
    stand.fire$species=="Psme",
    exp(-3.959 + 0.826 * log(stand.fire$basalarea_tree) + 0.175 * log(stand.fire$count_tree)),
    exp(-4.824 + 0.804 * log(stand.fire$basalarea_tree) + 0.333 * log(stand.fire$count_tree))))


# calculate crown ratio
# for top height use p90 height unless this is 0, then use mean tree height
stand.fire$cr = ifelse(
  stand.fire$height_p90 >0,
  (stand.fire$height_p90 - stand.fire$cbh)/stand.fire$height_p90,  # ratio of total crown height to dominant canopy height
  (stand.fire$height_mean_tree - stand.fire$cbh)/stand.fire$height_mean_tree
)

# set cr to 0 if NA
stand.fire[is.na(stand.fire$cr),]$cr = 0


# # create export df, drop extra carbon outputs
# stand.export = stand.fire %>%
#   select(-regeneration_c, -litter_c_ag, -downedWood_c_ag)

#####
# 6. calc mean values for treated and untreated
#####

stand.means = stand.fire %>%
  filter(rid %in% env.rids) %>% # subset to study area
  mutate(mgmt = ifelse(rid %in% house.rids,"treated","untreated")) %>%  # assign trt 
  ungroup() %>%  # ungroup so can drop rid
  dplyr::select(-c(rid,species,fire_windspeed,fire_spread,x,y)) %>%  # only select numeric values of interest
  group_by(year,fire_year,mgmt) %>%  # sum by year
  add_tally(name="n_ha") %>%  # add tally
  summarise_all(mean) 
  

#####
# 7. outputs
#####

# write outputs
write.csv(stand.fire, paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_stand_",rep,".csv"), row.names=FALSE)
write.csv(landscape, paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_landscape_",rep,".csv"), row.names=FALSE)
write.csv(fire.in, paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_fire_",rep,".csv"), row.names=FALSE)
write.csv(stand.means, paste0("NR/output_processed/",forest_type,"_",clim,"_",scen,"_",prop,"_standmeans_",rep,".csv"), row.names=FALSE)


print(paste0(filename," output prep complete"))
