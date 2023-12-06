#!/bin/bash

# bash script to run iland for a list of scenarios
# on the htc cluster.
# script runs from directory above NR and
# takes three arguments, which must be in correct order.
# usage: bash scripts/run_iland/iland_run.sh [gcm] [scen] [rep]

#####
# setup
#####

# bring in everything from gluster
cp /mnt/gluster/braziunas/iland_job/nr.tar.gz ./
cp /mnt/gluster/braziunas/iland_job/qt.tar.gz ./
cp /mnt/gluster/braziunas/iland_job/R.tar.gz ./

# untar qt, iland, r
tar -xzf ilandbuild.tar.gz
tar -xzf qt.tar.gz
tar -xzf nr.tar.gz
tar -xzf R.tar.gz

# set up paths for R
export PATH=$(pwd)/R/bin:$PATH
export RHOME=$(pwd)/R

# set up paths for qt, copy to iland
cd qt
export PATH=$(pwd)/bin:$PATH
export LD_LIBRARY_PATH=$(pwd)/lib:$LD_LIBRARY_PATH
cp -r plugins/sqldrivers/ ../build/ilandc/
cd bin

# create qt.conf file
echo [PATHS] >> qt.conf
echo Prefix = $_CONDOR_SCRATCH_DIR/qt >> qt.conf

# back to main dir
cd ../../

#####
# arguments and other variables
#####

gcm=$1
scen=$2
rep=$3

# other variables, which are fixed across scenarios

forest_type="subalpine"

simulation_years="120"

# assign appropriate array with KBDI ref values
if [ $forest_type = "subalpine" ]
then
    # associative array
    declare -A gcms=(["CanESM2"]="0.208" ["HadGEM2CC"]="0.202" ["HadGEM2ES"]="0.200")
else
    exit
fi

#####
# run iland and reduce outputs with R
#####

cd NR # move to project file directory

# assign appropriate project file
if [ $scen = "nomgmt_p0" ]
then
    # assign nomgmt
    xml=${forest_type}_nomgmt.xml
else
    # else assign mgmt
    xml=${forest_type}_mgmt.xml
fi

# copy appropriate NLM map only if mgmt scenario
if [ $xml = "subalpine_mgmt.xml" ]
then
    cp gis/mgmt/${scen}_${rep}.txt gis/mgmt/scenario_map.txt
    cp gis/mgmt/${scen}_${rep}.csv gis/mgmt/scenario_rids.csv
fi

# run each scenario with custom kbdi ref
../build/ilandc/ilandc ${xml} $simulation_years \
model.world.environmentFile=gis/${forest_type}_${gcm}_environment.txt \
system.database.out=${forest_type}_${gcm}_${scen}_output_${rep}.sqlite \
modules.fire.KBDIref="${gcms[${gcm}]}"

# reduce outputs with R code
cd .. # back to main directory

Rscript NR/scripts/iland_output_prep.R
Rscript NR/scripts/fire_intensity.R

#####
# export outputs and clean up
#####

# tar outputs
tar -czf ${forest_type}_${gcm}_${scen}_output_${rep}.tar.gz NR/output_processed/

# update: tar should transfer back to home folder per chtc if < 4 GB in size
# # move to gluster
# cp ${forest_type}_${gcm}_${scen}_output_${rep}.tar.gz /mnt/gluster/braziunas/

# delete tarballs that are not outputs
rm nr.tar.gz
rm qt.tar.gz
rm R.tar.gz
rm ilandbuild.tar.gz
