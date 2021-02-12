#####
#
## combined data summaries from iland and fire risk outputs
#
#####

rm(list=ls())

# wd inherited from project

# # libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(zoo)


#####
# 1. load landscape and stand data
#####

path.in = "outputs/iland_outputs/runs_processed/"

### load in landscape and landscapefire data for all scenarios
# incorporate cumulative area burned, cumulative hi area burned, cumulative proportion hi

landscape.in = data.frame()

for(i in list.files(path=path.in, pattern="landscape")) {
  file.name = strsplit(i, split="_")
  gcm = file.name[[1]][2]
  config = file.name[[1]][3]
  amt = file.name[[1]][4]
  rep = strsplit(file.name[[1]][6], split=".csv")[[1]][1]
  land.temp = read.csv(paste0(path.in,i))
  
  # rectify differences in names between landscape and landscapefire
  names(land.temp) = c("year","variable","value")
  
  # calc cumulative area burned
  c.burn = land.temp %>%
    filter(variable=="burned") %>%
    arrange(year) %>%  # order by year
    mutate(cum_burned = cumsum(value),
           variable="cum_burned") %>% # cumulative sum
    dplyr::select(year,variable,cum_burned) %>%
    rename(value=cum_burned)
  
  # calc cumulative high intensity fire
  c.hi = land.temp %>%
    filter(variable=="high") %>%
    arrange(year) %>%  # order by year
    mutate(cum_high = cumsum(value),
           variable="cum_high") %>%  # cumulative sum
    dplyr::select(year,variable,cum_high) %>%
    rename(value=cum_high)
  
  # calc cumulative high intensity fire
  c.prophi = c.burn %>%
    dplyr::select(-variable) %>%
    rename(cum_burned=value) %>%
    left_join(c.hi, by="year") %>%
    rename(cum_hi = value) %>%
    mutate(variable="cum_prophi",
           value=ifelse(!is.na(cum_hi/cum_burned),cum_hi/cum_burned,0)) %>%
    dplyr::select(year,variable,value)
  
  # bind to land.temp
  land.full = rbind(land.temp,c.burn,c.hi,c.prophi)

  
  landscape.in = rbind(landscape.in, data.frame(cbind(land.full, gcm=gcm, config=config,amt=amt, rep=rep)))
}


### load mean stand structure data, landscape level, all scenarios
stand.in = data.frame()

for(i in list.files(path=path.in, pattern="_standmeans_")) {
  file.name = strsplit(i, split="_")
  gcm = file.name[[1]][2]
  config = file.name[[1]][3]
  amt = file.name[[1]][4]
  rep = strsplit(file.name[[1]][6], split=".csv")[[1]][1]
  stand.temp = read.csv(paste0(path.in,i))
  
  stand.in = rbind(stand.in, data.frame(cbind(stand.temp, gcm=gcm, config=config,amt=amt,rep=rep)))
}


# convert dead carbon pools to biomass in Mg/ha
stand.bm = stand.in %>%
  mutate(regeneration_mg_ha = regeneration_c * 2 * 0.001,
         downedWood_mg_ha = downedWood_c_ag * 2 * 0.001,
         litter_mg_ha = litter_c_ag * 2 * 0.001) %>%
  dplyr::select(-c(regeneration_c,downedWood_c_ag,litter_c_ag))

#####
# 2. load and summarize annual fire risk data
#####

### load fire risk data all scenarios

dspace.in = read.csv("outputs/analysis/fire_risk/dspace.csv")
ssz.in = read.csv("outputs/analysis/fire_risk/szone.csv")
lmet.in = read.csv("outputs/analysis/fire_risk/landscapemetrics.csv")


### defensible space
head(dspace.in)

# first for each nomgmt scenario, we randomly selected a trtment map and
# used this to calculate comparison values

# compare fire intensity (flame length) in trted hectares vs nomgmt scenario (randomly
# selected ha using NLMs from trt scenarios)

dspace = dspace.in %>%
  filter(mgmt=="treated") # filter dspace for only ha labeled as trted

### ssz

head(ssz.in)

# here for the safe suppression zone fire risk metric
# this is essentially quantifying the exposure of structures to
# high intensity fire as:
# the proportion of structures exposed to any (> 1 ha) high intensity fire
# within their the 9-ha SSZ per year
# if no structure is exposed in a given year, this = 0

# so step one is creating an entry for every year
scens.full = expand.grid(fire_year=seq(1981,2099,1),
                          # mgmt=c("treated","untreated"),
                          clim=c("CanESM2","HadGEM2CC","HadGEM2ES"),
                          scen_group=c("nomgmt_p0","random_p10","random_p30","random_p50",
                                       "cluster_p10","cluster_p30","cluster_p50"),
                         rep=seq(1,20,1))

# add in 0s
ssz = ssz.in %>%
  mutate(scen_group=paste0(scen,"_",prop)) %>%
  # add in 0 for years with no fire
  right_join(scens.full, by=c("fire_year","clim","scen_group","rep")) %>%
  # deal with NAs caused by joining
  dplyr::select(-c(scen,prop)) %>%
  mutate(scen_grp = scen_group) %>%
  separate(scen_grp, c("scen","prop")) 

# remove na values
ssz[is.na(ssz)] = 0


### landscape metrics

head(lmet.in)

# similar to ssz, need to add 0s for years with no fire
scens.full2 = expand.grid(fire_year=seq(1981,2099,1),
                          # mgmt=c("treated","untreated"),
                          clim=c("CanESM2","HadGEM2CC","HadGEM2ES"),
                          scen_group=c("nomgmt_p0","random_p10","random_p30","random_p50",
                                       "cluster_p10","cluster_p30","cluster_p50"),
                          rep=seq(1,20,1),
                          class_name=c("unburned","low_moderate","high"),
                          metric=c("pland","lpi","area_am"))

# add in 0s
lmet = lmet.in %>%
  mutate(scen_group=paste0(scen,"_",prop)) %>%
  # add in 0 for years with no fire
  right_join(scens.full2, by=c("fire_year","clim","scen_group","rep","class_name","metric")) %>%
  # deal with NAs caused by joining
  dplyr::select(-c(scen,prop)) %>%
  mutate(scen_grp = scen_group) %>%
  separate(scen_grp, c("scen","prop")) 

# replace na values
lmet[is.na(lmet)] = 0

#####
# 3. annual output data summaries
#####

### annual forest characteristics, area burned, fire risk
# combining landscape, dspace, ssz, and lmet
# save landscape forest characteristics for data deposit
unique(landscape.in$variable)

forest.summary = landscape.in %>%
  pivot_wider(id_cols=c(year,gcm,config,amt,rep),names_from="variable",values_from = "value") %>%
  # omit columns not analyzed, reorder columns
  dplyr::select(c(year:old,burned,high,cum_burned,cum_high,fire_kbdi)) %>%
  # rename columns, include units
  rename(configuration=config, amount_pct=amt, forest_ha=forest, nonforest_ha=nonforest,
         pico_ha=pico, abla_ha=abla, pien_ha=pien, psme_ha=psme, potr_ha=potr, mixed_ha=mixed,
         age_lt_40_ha=young, age_40_100_ha=mid, age_100_250_ha=midold, age_gt_250_ha=old,
         total_burned_ha=burned, high_burned_ha=high, cumulative_burned_ha=cum_burned,
         cumulative_high_ha=cum_high, average_kdbi=fire_kbdi) 

# then get fire risk variables into consistent format for joining
head(dspace)
dspace.summ = dspace %>%
  # select only relevant columns
  dplyr::select(c(fire_year,tot_ha:rep)) %>%
  # rename to match, include units
  rename(year=fire_year, gcm=clim, configuration=scen, amount_pct=prop,
         hiz_burned_ha=ha_burned,mean_hiz_fl_m=fl_mean)

head(ssz)
ssz.summ = ssz %>%
  # select only relevant columns
  dplyr::select(fire_year,any_ssz_hi:rep,scen,prop) %>%
  # rename to match, include units
  rename(year=fire_year, gcm=clim, configuration=scen, amount_pct=prop, ssz_exposure_pct=any_ssz_hi) %>%
  # update ssz units from proportion to pct
  mutate(ssz_exposure_pct = ssz_exposure_pct*100)

head(lmet)
lmet.summ = lmet %>%
  # only high intensity class
  filter(class_name=="high") %>%
  # select only relevant columns
  dplyr::select(metric:value,fire_year,clim:rep,scen,prop) %>%
  # pivot wider to get each metric in own column
  pivot_wider(id_cols=c(fire_year:prop),names_from="metric",values_from="value") %>%
  # rename to match, include units
  rename(year=fire_year, gcm=clim, configuration=scen, amount_pct=prop, pland_high_pct=pland, lpi_high_ha = lpi,
         area_am_high_ha=area_am) %>%
  # update lpi units from pct to ha
  mutate(lpi_high_ha = lpi_high_ha/100 * 10000)

# combine in master landscape output
land.summary = forest.summary %>%
  mutate(rep=as.numeric(rep)) %>%
  full_join(dspace.summ, by=c("year","gcm","configuration","amount_pct","rep")) %>%
  dplyr::select(-c(tot_ha)) %>%
  full_join(ssz.summ, by=c("year","gcm","configuration","amount_pct","rep")) %>%
  full_join(lmet.summ, by=c("year","gcm","configuration","amount_pct","rep")) %>%
  # relabel config and amount categories, replace NA with 0s for HIZ burned ha
  mutate(configuration=as.factor(ifelse(as.character(configuration)=="cluster","clustered",
                                        ifelse(as.character(configuration)=="random","dispersed","no_treatment"))),
         amount_pct=as.factor(ifelse(as.character(amount_pct)=="p10",10,
                           ifelse(as.character(amount_pct)=="p30",30,
                                  ifelse(as.character(amount_pct)=="p50",50,0)))),
         hiz_burned_ha =ifelse(is.na(hiz_burned_ha),0,hiz_burned_ha))

summary(land.summary)

write.csv(land.summary,"outputs/analysis/output_summaries/annual_fire_forest_risk.csv",row.names=FALSE)

### annual fuels by fuels treatment category

head(stand.bm)

fuels.summary = stand.bm %>%
  # select only relevant columns, reorder
  dplyr::select(year,fire_year,gcm:rep,mgmt,n_ha,regeneration_mg_ha,litter_mg_ha,downedWood_mg_ha,cbh:cfl) %>%
  # rename, add units
  rename(configuration=config, amount_pct=amt, fuels_treatment=mgmt, woody_mg_ha=regeneration_mg_ha, forestFloor_mg_ha=litter_mg_ha,
         cbh_m=cbh, cbd_kg_m3=cbd, cfl_kg_m2=cfl) %>%
  mutate(configuration=as.factor(ifelse(as.character(configuration)=="cluster","clustered",
                                        ifelse(as.character(configuration)=="random","dispersed","no_treatment"))),
         amount_pct=as.factor(ifelse(as.character(amount_pct)=="p10",10,
                                     ifelse(as.character(amount_pct)=="p30",30,
                                            ifelse(as.character(amount_pct)=="p50",50,0)))))

write.csv(fuels.summary,"outputs/analysis/output_summaries/annual_fuels_treatment_comparison.csv",row.names=FALSE)

#####
# 4. summary fire risk over duration of each replicate
#####

### dspace
# average over all years, so 1 n per scenario
dspace.dur = dspace.summ %>%
  dplyr::select(-c(year)) %>%
  # idea is mean flame length for ha that burn, so need to weight
  # mean by number of ha that burned in a given year
  mutate(fl_mean_wt = hiz_burned_ha * mean_hiz_fl_m) %>%
  group_by(gcm,configuration,amount_pct,rep,tot_ha) %>%
  # step 1: sum up tot ha burned, sum weighted fl, also include maximum fl_mean for each scenario
  summarise(hiz_burned_ha = sum(hiz_burned_ha), fl_mean_wt = sum(fl_mean_wt),
            max_hiz_fl_m = max(mean_hiz_fl_m)) %>%
  # step 2: divide wtd sum by tot to get mean
  mutate(mean_hiz_fl_m = fl_mean_wt/hiz_burned_ha) %>%
  dplyr::select(-fl_mean_wt) %>% # drop wted sum
  rename(treated_ha = tot_ha) %>%
  # reorder columns
  dplyr::select(c(gcm:hiz_burned_ha,mean_hiz_fl_m,max_hiz_fl_m))

### ssz
# average over all years, so 1 n per scenario
ssz.dur = ssz.summ %>%
  dplyr::select(-c(year)) %>%
  group_by(gcm,configuration,amount_pct,rep) %>%
  summarise(mean_ssz_exposure_pct = mean(ssz_exposure_pct),
            max_ssz_exposure_pct = max(ssz_exposure_pct))


### landscape
# average over all years, so 1 n per scenario
lmet.dur = lmet.summ %>%
  dplyr::select(-year) %>%
  group_by(gcm,configuration,amount_pct,rep) %>%
  summarise(mean_pland_high_pct = mean(pland_high_pct),
            max_pland_high_pct = max(pland_high_pct),
            mean_lpi_high_ha = mean(lpi_high_ha),
            max_lpi_high_ha = max(lpi_high_ha),
            mean_area_am_high_ha = mean(area_am_high_ha),
            max_area_am_high_ha = max(area_am_high_ha)) 


### summary fire risk over duration
# summarize from landscape level
frisk.summary = dspace.dur %>%
  full_join(ssz.dur, by=c("gcm","configuration","amount_pct","rep")) %>%
  full_join(lmet.dur, by=c("gcm","configuration","amount_pct","rep")) %>%
  ungroup() %>%
  mutate(configuration=as.factor(ifelse(as.character(configuration)=="cluster","clustered",
                                        ifelse(as.character(configuration)=="random","dispersed","no_treatment"))),
         amount_pct=as.factor(ifelse(as.character(amount_pct)=="p10",10,
                                     ifelse(as.character(amount_pct)=="p30",30,
                                            ifelse(as.character(amount_pct)=="p50",50,0)))))
  
write.csv(frisk.summary,"outputs/analysis/output_summaries/summary_fire_risk.csv",row.names=FALSE)
