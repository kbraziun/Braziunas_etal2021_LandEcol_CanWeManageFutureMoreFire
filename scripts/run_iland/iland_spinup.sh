#!/bin/bash
# bash script to run iland for a list of spinup scenarios
# script should be run from the main directory, which is the
# directory containing the readme
# script takes any number of args, which must be the names of
# iland project files
# usage: bash scripts/run_iland/iland_spinup.sh [project_files]

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

#####
# set current path
#####

path_start=$(pwd)

#####
# output location
#####

if [ ! -d outputs/iland_outputs/spinup ]
then
    mkdir outputs/iland_outputs/spinup
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

        # run iland with custom environment, output, kbdi ref
        ../executable_qt512/ilandc.exe $xml 300 \
        system.database.out=${forest_type}_${gcm}_spinup_output.sqlite \
        model.world.environmentFile=gis/${forest_type}_${gcm}_environment.txt \
        modules.fire.KBDIref="${gcms[${gcm}]}"

        # rename and move outputs
        mv output/${forest_type}_${gcm}_spinup_output.sqlite ../../../outputs/iland_outputs/spinup/
        mv output/spinup_nfire.txt ../../../outputs/iland_outputs/spinup/${forest_type}_${gcm}_spinup_nfire.txt

        # rename snapshot
        mv init/spinup_300.sqlite init/${forest_type}_${gcm}_spinup_300.sqlite
        mv init/spinup_300.asc init/${forest_type}_${gcm}_spinup_300.asc
    
    done
    
done
