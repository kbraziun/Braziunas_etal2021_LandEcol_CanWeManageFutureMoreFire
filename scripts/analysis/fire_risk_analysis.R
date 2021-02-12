#####
#
## fire risk analysis
#
#####

rm(list=ls())

# wd inherited from project

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(plotrix)
library(lme4)
library(lmerTest)
library(MuMIn)

#####
# 1. read in and prep fire risk data
#####

# read in scenario summaries
frisk.in = read.csv("outputs/analysis/output_summaries/summary_fire_risk.csv")

### research question: when defensible spac treatments are applied around all houses
# which scenarios of WIU housing amount and configuration minimize fire risk?

# to answer, first removing nomgmt
# then will fit linear mixed effects model for each fire risk metric in subsequent sections

frisk.drop = frisk.in %>%
  filter(configuration !="no_treatment") %>%
  # set factor order
  mutate(configuration = factor(configuration, levels=c("dispersed","clustered")),
         gcm = factor(gcm, levels=c("HadGEM2CC","CanESM2","HadGEM2ES")))

head(frisk.in)
str(frisk.in)

#####
# 2. home ignition zone (defensible space)
#####

### first use fixed model to look at assumptions
fixed.dspace = lm((mean_hiz_fl_m)~configuration+amount_pct+configuration:amount_pct+gcm,data=frisk.drop)

# check assumptions
plot(fixed.dspace)  
qqPlot(fixed.dspace)
boxCox(fixed.dspace) # transform recommended, but does not improve meeting assumptions

### next fit linear mixed effects model with amount, configuration, and their interaction
# as fixed effects and gcm as random effect
fullmgmt.dspace = lmer((mean_hiz_fl_m)~configuration+amount_pct+configuration:amount_pct+(1|gcm),REML=FALSE, data=frisk.drop)
# also fit reduced model without gcm
redmgmt.dspace = lm((mean_hiz_fl_m)~configuration+amount_pct+configuration:amount_pct, data=frisk.drop)

# check assumptions
plot(fullmgmt.dspace)
qqPlot(residuals(fullmgmt.dspace))

# view results
summary(fullmgmt.dspace)  # coefficients
r.squaredGLMM(fullmgmt.dspace)  # variance explained

# likelihood ratio test for effect of gcm
anova(fullmgmt.dspace,redmgmt.dspace)

#####
# 3. safe suppression zone
#####

### first use fixed model to look at assumptions
fixed.ssz = lm((mean_ssz_exposure_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct+gcm,data=frisk.drop)

# check assumptions
plot(fixed.ssz)  
qqPlot(fixed.ssz)
boxCox(fixed.ssz)  # used to choose transformation

### next fit linear mixed effects model with amount, configuration, and their interaction
# as fixed effects and gcm as random effect
fullmgmt.ssz = lmer((mean_ssz_exposure_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct+(1|gcm),REML=FALSE, data=frisk.drop)
# also fit reduced model without gcm
redmgmt.ssz = lm((mean_ssz_exposure_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct, data=frisk.drop)

# check assumptions
plot(fullmgmt.ssz)
qqPlot(residuals(fullmgmt.ssz))

# view results
summary(fullmgmt.ssz)  # coefficients
r.squaredGLMM(fullmgmt.ssz)  # variance explained

# likelihood ratio test for effect of gcm
anova(fullmgmt.ssz,redmgmt.ssz)

#####
# 4. largest patch index
#####

### first use fixed model to look at assumptions
fixed.lpi = lm((mean_lpi_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct+gcm,data=frisk.drop)

# check assumptions
plot(fixed.lpi)  
qqPlot(fixed.lpi)
boxCox(fixed.lpi)  # used to choose transformation

### next fit linear mixed effects model with amount, configuration, and their interaction
# as fixed effects and gcm as random effect
fullmgmt.lpi = lmer((mean_lpi_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct+(1|gcm),REML=FALSE, data=frisk.drop)
# also fit reduced model without gcm
redmgmt.lpi = lm((mean_lpi_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct, data=frisk.drop)

# check assumptions
plot(fullmgmt.lpi)
qqPlot(residuals(fullmgmt.lpi))

# view results
summary(fullmgmt.lpi)  # coefficients
r.squaredGLMM(fullmgmt.lpi)  # variance explained

# likelihood ratio test for effect of gcm
anova(fullmgmt.lpi,redmgmt.lpi)

#####
# 5. area-weighted mean patch size
#####

### first use fixed model to look at assumptions
fixed.am = lm((mean_area_am_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct+gcm,data=frisk.drop)

# check assumptions
plot(fixed.am)  
qqPlot(fixed.am)
boxCox(fixed.am)  # used to choose transformation

### next fit linear mixed effects model with amount, configuration, and their interaction
# as fixed effects and gcm as random effect
fullmgmt.am = lmer((mean_area_am_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct+(1|gcm),REML=FALSE, data=frisk.drop)
# also fit reduced model without gcm
redmgmt.am = lm((mean_area_am_high_ha)^(1/2)~configuration+amount_pct+configuration:amount_pct, data=frisk.drop)

# check assumptions
plot(fullmgmt.am)
qqPlot(residuals(fullmgmt.am))

# view results
summary(fullmgmt.am)  # coefficients
r.squaredGLMM(fullmgmt.am)  # variance explained

# likelihood ratio test for effect of gcm
anova(fullmgmt.am,redmgmt.am)

#####
# 6. pland
#####

### first use fixed model to look at assumptions
fixed.pland = lm((mean_pland_high_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct+gcm,data=frisk.drop)

# check assumptions
plot(fixed.pland)  
qqPlot(fixed.pland)
boxCox(fixed.pland)  # used to choose transformation

### next fit linear mixed effects model with amount, configuration, and their interaction
# as fixed effects and gcm as random effect
fullmgmt.pland = lmer((mean_pland_high_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct+(1|gcm),REML=FALSE, data=frisk.drop)
# also fit reduced model without gcm
redmgmt.pland = lm((mean_pland_high_pct)^(1/2)~configuration+amount_pct+configuration:amount_pct, data=frisk.drop)

# check assumptions
plot(fullmgmt.pland)
qqPlot(residuals(fullmgmt.pland))

# view results
summary(fullmgmt.pland)  # coefficients
r.squaredGLMM(fullmgmt.pland)  # variance explained

# likelihood ratio test for effect of gcm
anova(fullmgmt.pland,redmgmt.pland)

#####
# 7. data summaries, magnitude of change compared to no treatment
#####

### summary statistics for each GCM
# outputs reported in Table A1

# maximum fire risk across all replicates
frisk.max = frisk.in %>%
  dplyr::select(c(gcm:amount_pct,starts_with("max"))) %>%
  group_by(gcm,configuration,amount_pct) %>%
  summarise_all(max)
  
# calculate baseline for no treatment scenario
frisk.notrt = frisk.in %>%
  dplyr::select(c(gcm,configuration),starts_with("mean")) %>%
  filter(configuration == "no_treatment") %>%
  pivot_longer(cols=starts_with("mean"),
               names_to="frisk_met",
               values_to="value") %>%
  group_by(gcm, frisk_met) %>%
  summarise(mean_frisk_nomgmt = mean(value),
            var_frisk_nomgmt = var(value))

# difference in fire risk due to treatment scenarios
frisk.out = frisk.in %>%
  dplyr::select(c(gcm,configuration,amount_pct),starts_with("mean")) %>%
  pivot_longer(cols=starts_with("mean"),
               names_to="frisk_met",
               values_to="value") %>%
  group_by(gcm,configuration,amount_pct,frisk_met) %>%
  # summary stats
  summarise(mean_frisk = mean(value), 
            se_frisk=std.error(value), 
            var_frisk=var(value), 
            n_obs = n()) %>%
  # join with notrt 
  left_join(frisk.notrt, by=c("gcm","frisk_met")) %>%
  # calculate difference, se, and approximate CI based on unequal variance, Welch-Satterthwaite T-statistic
  mutate(diff_mgmt = mean_frisk - mean_frisk_nomgmt, 
         se_diff_mgmt = sqrt(var_frisk_nomgmt/n_obs + var_frisk/n_obs),
         adf = ((var_frisk_nomgmt/n_obs + var_frisk/n_obs)^2)/(((var_frisk_nomgmt/n_obs)^2)/(n_obs-1) + ((var_frisk/n_obs)^2)/(n_obs-1)),
         t = qt(0.975,adf),
         ci = t * se_diff_mgmt,
         ci_lower = diff_mgmt - ci,
         ci_upper = diff_mgmt + ci) %>%
  # pct difference
  mutate(diff_mgmt_pct = 100*diff_mgmt/mean_frisk_nomgmt,
         se_diff_mgmt_pct = 100*se_diff_mgmt/mean_frisk_nomgmt,
         ci_diff_mgmt_pct = 100*ci/mean_frisk_nomgmt)
  

### summary statistics across all GCMs
# outputs are plotted in fig 6

# calculate baseline for no treatment scenario
allgcm.notrt = frisk.in %>%
  dplyr::select(configuration,starts_with("mean")) %>%
  filter(configuration == "no_treatment") %>%
  pivot_longer(cols=starts_with("mean"),
               names_to="frisk_met",
               values_to="value") %>%
  group_by(frisk_met) %>%
  summarise(mean_frisk_nomgmt = mean(value),
            var_frisk_nomgmt = var(value))

# difference in fire risk due to treatment scenarios
allgcm.out = frisk.in %>%
  dplyr::select(c(configuration,amount_pct),starts_with("mean")) %>%
  pivot_longer(cols=starts_with("mean"),
               names_to="frisk_met",
               values_to="value") %>%
  group_by(configuration,amount_pct,frisk_met) %>%
  # summary stats
  summarise(mean_frisk = mean(value), 
            se_frisk=std.error(value), 
            var_frisk=var(value), 
            n_obs = n()) %>%
  # join with notrt 
  left_join(allgcm.notrt, by=c("frisk_met")) %>%
  # calculate difference, se, and approximate CI based on unequal variance, Welch-Satterthwaite T-statistic
  mutate(diff_mgmt = mean_frisk - mean_frisk_nomgmt, 
         se_diff_mgmt = sqrt(var_frisk_nomgmt/n_obs + var_frisk/n_obs),
         adf = ((var_frisk_nomgmt/n_obs + var_frisk/n_obs)^2)/(((var_frisk_nomgmt/n_obs)^2)/(n_obs-1) + ((var_frisk/n_obs)^2)/(n_obs-1)),
         t = qt(0.975,adf),
         ci = t * se_diff_mgmt,
         ci_lower = diff_mgmt - ci,
         ci_upper = diff_mgmt + ci) %>%
  # pct difference
  mutate(diff_mgmt_pct = 100*diff_mgmt/mean_frisk_nomgmt,
         se_diff_mgmt_pct = 100*se_diff_mgmt/mean_frisk_nomgmt,
         ci_diff_mgmt_pct = 100*ci/mean_frisk_nomgmt)

#####
# 8. extra analyses
#####

### number of fire years in which >1/4 landscape burned in HadGEM2 scenarios

land.in = read.csv("outputs/analysis/output_summaries/annual_fire_forest_risk.csv")

lfires = land.in %>%
  # mean across replicates
  group_by(gcm,configuration,amount_pct,year) %>%
  summarise(total_burned_ha = mean(total_burned_ha)) %>%
  # tally times fire burned more than 1/4 of landscape (2500 ha)
  filter(configuration=="no_treatment", gcm %in% c("HadGEM2CC","HadGEM2ES"), 
         year>=2050, total_burned_ha>=2500) %>%
  group_by(gcm) %>%
  tally()

lfires

### when did high intensity fire peak from loess

# prep data
hi.in = land.in %>%
  filter(configuration=="no_treatment",
         !is.na(high_burned_ha))
  
hadcc.model = loess(high_burned_ha ~ year, data=hi.in[hi.in$gcm=="HadGEM2CC",], span=100/119)
hadcc.vals = data.frame(x=seq(1981,2099,1),y=predict(hadcc.model, data.frame(year=seq(1981,2099,1))))

hades.model = loess(high_burned_ha ~ year, data=hi.in[hi.in$gcm=="HadGEM2ES",], span=100/119)
hades.vals = data.frame(x=seq(1981,2099,1),y=predict(hades.model, data.frame(year=seq(1981,2099,1))))

# check plots for consistency, some warnings due to plotting on log axis
ggplot(aes(y=high_burned_ha, x=year),data=hi.in[hi.in$gcm=="HadGEM2CC",]) +
  geom_point() +
  geom_line(aes(y=y,x=x),data=hadcc.vals) +
  scale_y_log10()

ggplot(aes(y=high_burned_ha, x=year),data=hi.in[hi.in$gcm=="HadGEM2ES",]) +
  geom_point() +
  geom_line(aes(y=y,x=x),data=hades.vals) +
  scale_y_log10()

# looks good

hadcc.vals[which.max(hadcc.vals$y),] # 2066
hades.vals[which.max(hades.vals$y),] #2065

### avg dwd fuel loads

# read in and mean value by year and scenario
fuels.in = read.csv("outputs/analysis/output_summaries/annual_fuels_treatment_comparison.csv") %>%
  filter(fuels_treatment=="untreated") %>%
  group_by(year,gcm,configuration,amount_pct) %>%
  summarise(mean_dwd = mean(downedWood_mg_ha))

min(fuels.in$mean_dwd) # 91.4
