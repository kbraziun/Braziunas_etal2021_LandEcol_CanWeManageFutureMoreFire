#!/bin/bash
# bash script to run iland for a list of scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes any number of args, which must be the names of
# iland project files
# usage: bash scripts/run_iland/iland_run_intensitytest.sh [project_files]

#####
# catch errors
#####
set -e # terminate if non-zero exit
set -u # terminate if variable unset
set -o pipefail # terminate if pipe command fails

#####
# arguments
#####

if [ $# -lt 1 ]
then
    echo "list of project files required"
    exit
else
    xml_list=$@
fi

reps=5

#####
# set current path
#####

path_start=$(pwd)

#####
# output locations
#####

if [ ! -d outputs/iland_outputs/runs ]
then
    mkdir outputs/iland_outputs/runs
fi

if [ ! -d programs/the_model/NR/output/fire ]
then
    mkdir programs/the_model/NR/output/fire
fi

#####
# loop through scripts
#####

for xml in $xml_list
do 
    cd $path_start # start back in main directory
    cd programs/the_model/NR # move to project file directory

    #####
    # set up variables for looping through gcms, kbdi ref
    #####

    # extract forest type from filename
    forest_type=$(basename $xml | cut -d"." -f 1 | cut -d"_" -f 1)
    
    # extract mgmt scenario from filename
    scen=$(basename $xml | cut -d"." -f 1 | cut -d"_" -f 2)

    # assign appropriate array with KBDI ref values
    if [ $forest_type = "subalpine" ]
    then
        # associative array
        declare -A gcms=(["CanESM2"]="0.208" ["HadGEM2CC"]="0.202" ["HadGEM2ES"]="0.200")
    else
        exit
    fi

    #####
    # run iland
    #####

    for gcm in "${!gcms[@]}"
    do

        for rep in $(seq 1 $reps)
        do
            # run iland with custom environment, output, kbdi ref
            ../executable_qt512/ilandc.exe $xml 25 \
            system.database.out=${forest_type}_${gcm}_${scen}_output_${rep}.sqlite \
            model.world.environmentFile=gis/${forest_type}_${gcm}_environment.txt \
            model.initialization.file=init/${forest_type}_CanESM2_spinup_300.sqlite \
            modules.fire.KBDIref="${gcms[${gcm}]}"

            # rename and move outputs
            mv output/${forest_type}_${gcm}_${scen}_output_${rep}.sqlite ../../../outputs/iland_outputs/runs/

            # make fire directory or
            # clear out previous runs if they exist
            if [ ! -d ../../../outputs/iland_outputs/runs/${forest_type}_${gcm}_${scen}_fire_${rep} ]
            then
                mkdir ../../../outputs/iland_outputs/runs/${forest_type}_${gcm}_${scen}_fire_${rep}
            else
                rm ../../../outputs/iland_outputs/runs/${forest_type}_${gcm}_${scen}_fire_${rep}/*
            fi

            mv output/fire/* ../../../outputs/iland_outputs/runs/${forest_type}_${gcm}_${scen}_fire_${rep}

        done
    
    done
    
done
