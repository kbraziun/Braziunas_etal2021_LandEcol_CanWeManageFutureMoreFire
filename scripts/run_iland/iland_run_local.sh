#!/bin/bash
# bash script to run iland for a list of scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes any number of args, which must be the names of
# iland project files
# usage: bash scripts/run_iland/iland_run_local.sh [reps]

#####
# catch errors
#####
set -e # terminate if non-zero exit
set -u # terminate if variable unset
set -o pipefail # terminate if pipe command fails

#####
# arguments and other variables
#####

if [ $# -lt 1 ]
then
    echo "number of reps required"
    exit
else
    reps=$@
fi

#####
# other variables
#####

forest_type="subalpine"

simulation_years="120"

#####
# loop through scenarios and reps
#####

# assign appropriate array with KBDI ref values
if [ $forest_type = "subalpine" ]
then
    # associative array
    declare -A gcms=(["CanESM2"]="0.208" ["HadGEM2CC"]="0.202" ["HadGEM2ES"]="0.200")
else
    exit
fi

# array with scenarios
declare -a scens=("nomgmt_p0" "random_p10" "random_p30" "random_p50" "cluster_p10" "cluster_p30" "cluster_p50")

#####
# run iland
#####

cd programs/the_model/NR # move to project file directory

for gcm in "${!gcms[@]}"
do

    for scen in "${scens[@]}"
    do

        # assign appropriate project file
        if [ $scen = "nomgmt_p0" ]
        then
            # assign nomgmt
            xml=${forest_type}_nomgmt.xml
        else
            # else assign mgmt
            xml=${forest_type}_mgmt.xml
        fi

        for rep in $(seq 1 $reps)
        do

            # copy appropriate NLM map only if mgmt scenario
            if [ $xml = "subalpine_mgmt.xml" ]
            then
                cp gis/mgmt/${scen}_${rep}.txt gis/mgmt/scenario_map.txt
                cp gis/mgmt/${scen}_${rep}.csv gis/mgmt/scenario_rids.csv
            fi

            # run each scenario with custom kbdi ref
            ../executable_qt512/ilandc.exe ${xml} $simulation_years \
            model.world.environmentFile=gis/${forest_type}_${gcm}_environment.txt \
            system.database.out=${forest_type}_${gcm}_${scen}_output_${rep}.sqlite \
            modules.fire.KBDIref="${gcms[${gcm}]}"

            # reduce outputs with R code
            cd .. # run from NR directory for relative paths
            Rscript.exe NR/scripts/iland_output_prep.R
            Rscript.exe NR/scripts/fire_intensity.R
            cd NR

            # clean up
            rm output/${forest_type}_${gcm}_${scen}_output_${rep}.sqlite
            rm output/fire/*

        done

    done

done

mv output_processed/* ../../../outputs/iland_outputs/runs_processed/
