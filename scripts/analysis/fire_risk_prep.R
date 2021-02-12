#####
#
## calculate fire risk metrics from iland run outputs
#
#####

rm(list=ls())

# wd inherited from project

# libraries
library(raster)
library(dplyr)
library(landscapemetrics)

#####
# 1. set up study area
#####

xmin = 0
xmax = 15000
ymin = 0
ymax = 15000
ncol=100
nrow=100
lsize=ncol*nrow

# study landscape will always be centered
sxmin = xmin + (xmax - ncol*100)/2
sxmax = xmax - (xmax - ncol*100)/2
symin = ymin + (ymax - nrow*100)/2
symax = ymax - (ymax - nrow*100)/2


# load env grid for all scenarios
env.in = read.table("programs/the_model/NR/gis/env_grid.txt", skip=6)

# using corner coords from ASCII/txt file to turn into raster
env.full = raster(as.matrix(env.in), xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax)

# also sub-level env, just study area
env.sub = crop(env.full, extent(sxmin,sxmax,symin,symax))

# get rid vals to match up with intensity
env.vals = values(env.sub)
env.coords = xyFromCell(env.sub, 1:ncell(env.sub))
env = data.frame(cbind(rid=env.vals,env.coords))

#####
# 2. read in files and rids
#####

path.in = "outputs/iland_outputs/runs_processed/"

# outputs
landscape.summall = data.frame()
dspace.summall = data.frame()
szone.summall = data.frame()


for(i in list.files(path=path.in, pattern="_fireintensity_")) {
  file.name = strsplit(i, split="_")
  clim = file.name[[1]][2]
  scen = file.name[[1]][3]
  prop = file.name[[1]][4]
  rep = strsplit(file.name[[1]][6], split=".csv")[[1]]
  fi.in = read.csv(paste0(path.in,i))
  
  # defensible space and ssz rids
  # for random of cluster, match up with appropriate map
  if (scen %in% c("random","cluster")) {
    
    house.rids = read.csv(paste0("programs/the_model/NR/gis/mgmt/",scen,"_",prop,"_",rep,".csv"), header=TRUE)[,1]
    ssz.rids = read.csv(paste0("programs/the_model/NR/gis/analysis/","ssz_rids_",scen,"_",prop,"_",rep,".csv"),header=TRUE)
    print(paste(scen,prop,rep,"has",length(house.rids),"houses"))
    print(paste("ssz ha should equal",9*length(house.rids),"and is",length(ssz.rids[,2])))
    
  }
  
  # for nomgmt, randomly choose a map
  if (scen %in% c("nomgmt")) {
    
    rscen = sample(c("random","cluster"),1)
    rprop = sample(c("p10","p30","p50"),1)
    rrep = sample(c(1:20),1)
    house.rids = read.csv(paste0("programs/the_model/NR/gis/mgmt/",rscen,"_",rprop,"_",rrep,".csv"), header=TRUE)[,1]
    ssz.rids = read.csv(paste0("programs/the_model/NR/gis/analysis/","ssz_rids_",rscen,"_",rprop,"_",rrep,".csv"),header=TRUE)
    print(paste("selected map for",scen,prop,rep,"has",length(house.rids),"houses"))
    print(paste("ssz ha should equal",9*length(house.rids),"and is",length(ssz.rids[,2])))
    
  }
  
  
  #####
  # 3. look at treated versus untreated defensible space
  #####
  
  fi.in$mgmt = ifelse(fi.in$rid %in% house.rids, "treated","untreated")
  
  # subset to variables of interest
  # add number ha treated and untreated in landscape
  
  dspace = fi.in %>%
    dplyr::select(fire_year,mgmt,flame_length) %>%
    mutate(count=1, 
           tot_ha = ifelse(mgmt=="treated",length(house.rids),lsize - length(house.rids)))
  
  # total ha burned, mean and median fil
  
  dspace.tot = dspace %>%
    # dplyr::select(fire_year,mgmt,fil,count,tot_ha_treated,tot_ha_untreated) %>%
    group_by(fire_year,mgmt,tot_ha) %>%
    summarise(ha_burned=sum(count),  # count up ha burned by mgmt, year
              # fil_mean =mean(fil), fil_median=median(fil), # mean, median fil left out for now
              fl_mean = mean(flame_length))  # mean flame length
  
  # add scenario, rep
  dspace.tot$clim = clim
  dspace.tot$scen = scen
  dspace.tot$prop = prop
  dspace.tot$rep = rep
  
  dspace.summall = rbind(dspace.summall, as.data.frame(dspace.tot))
  
  #####
  # 4. high intensity fire in safe suppression zone (8nbr)
  #####
  
  # metric calculated is the proportion of houses that are exposed to any
  # high intensity fire (1 ha or more) in their safe suppression zone
  
  # also include proportion of SSZ burned, proportion of SSZ burned at 
  # high intensity
  
  szone.scen = data.frame()
  
  # loop through each house rid
  for (j in house.rids) {
    # ssz rids for that house
    house.ssz = ssz.rids %>%
      filter(rid==j)
    
    # subset fi.in, sum area burned in ssz
    hi.ssz = fi.in %>%
      filter(rid %in% house.ssz[,2]) %>%
      mutate(count = 1) %>%  # add counter
      group_by(fire_year) %>%  # summarise high intensity fire by year
      filter(intensity_class=="high") %>%
      summarise(hi_ha_burned = sum(count))
    
    # sum hi intensity area burned
    fi.ssz = fi.in %>%
      filter(rid %in% house.ssz[,2]) %>%
      mutate(count = 1) %>%  # add counter
      group_by(fire_year) %>%  # summarise fire by year
      summarise(ha_burned = sum(count)) %>%
      left_join(hi.ssz, by="fire_year")
    
    # replace nas with 0
    fi.ssz[is.na(fi.ssz)] = 0

    szone.scen = rbind(szone.scen,fi.ssz)
    
  }
  
  # summarize by year
  # total area in ssz, includes double couting
  ssz.area = length(house.rids)*9
  
  szone.tot = szone.scen %>%
    # add 1 for any ssz exposure
    mutate(any_hi_burned = ifelse(hi_ha_burned>0,1,0)) %>%
    group_by(fire_year) %>%
    summarise(prop_burned = sum(ha_burned)/ssz.area,
              prop_hi_burned = sum(hi_ha_burned)/ssz.area,
              any_ssz_hi = sum(any_hi_burned)/length(house.rids))
  
  # add scenario, rep
  szone.tot$clim = clim
  szone.tot$scen = scen
  szone.tot$prop = prop
  szone.tot$rep = rep
  
  # add to summary df
  
  szone.summall = rbind(szone.summall, as.data.frame(szone.tot))
  
  
  #####
  # 5. landscape metrics for hi fire
  #####
  
  # iterate through each year
  
  lmets.all = data.frame()
  
  for (k in min(fi.in$fire_year):max(fi.in$fire_year)) {

    land = fi.in %>%
      filter(fire_year==k) %>%
      mutate(intensity = ifelse(intensity_class=="high",2,1)) %>%  # assign numeric class for raster
      dplyr::select(rid,intensity) %>%
      right_join(env, by="rid") %>%  # join to all cells needed for raster, all coordinates
      dplyr::select(-rid)
    
    # replace na with 0
    land[is.na(land)] = 0
    
    # prepare to rasterize
    coordinates(land) = ~x + y
    gridded(land) = TRUE
    
    # create raster to analyze landscape metrics
    rland = raster(land)
    
    # class metrics pland, lpi, mean patch size
    lmets = data.frame(rbind(lsm_c_pland(rland),
                             lsm_c_lpi(rland),
                             lsm_c_area_mn(rland),
                             lsm_c_np(rland))) %>%
      dplyr::select(-c(layer,level,id))  # remove unneeded columns
    
    # calculate area-weighted mean patch size
    pmets = lsm_p_area(rland)  # get area of each patch
    am.calc = lmets %>%
      filter(metric == "pland") %>%  # use pland to get total class area
      mutate(class_area = value/100 * 10000) %>%  # get class area in ha
      dplyr::select(class,metric,class_area) %>%
      right_join(pmets, by="class") %>%  # join patch and class area
      # step 1: multiple patch area times proportional abundance within class
      # (i.e., patch area divided by class area)
      mutate(area_am_patch = value * (value/class_area))  %>% 
      group_by(class) %>%
      # step 2: sum to get area-weighted mean
      summarise(metric = "area_am", value = sum(area_am_patch))  
    
    lmets.out = rbind(lmets,am.calc)
    
    lmets.out$fire_year = k
    lmets.out$class_name = ifelse(lmets.out$class==0,"unburned",
                              ifelse(lmets.out$class==1,"low_moderate","high"))
    
    
    
    lmets.all = rbind(lmets.all,lmets.out)
        
  }
  
  # add scenario, rep
  lmets.all$clim = clim
  lmets.all$scen = scen
  lmets.all$prop = prop
  lmets.all$rep = rep

  landscape.summall = rbind(landscape.summall,lmets.all)
  

  print(paste("done with",clim,scen,prop,rep, sep=" "))
  
}



#####
# 6. write outputs
#####

write.csv(landscape.summall, file="outputs/analysis/fire_risk/landscapemetrics.csv", row.names=FALSE)
write.csv(dspace.summall, file="outputs/analysis/fire_risk/dspace.csv", row.names=FALSE)
write.csv(szone.summall, file="outputs/analysis/fire_risk/szone.csv", row.names=FALSE)

