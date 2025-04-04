#!/usr/bin/bash

# Define the range of years
start_year=2002
end_year=2018
biome=${1:-""}

# Create an array to hold the commands
commands=()

# Generate the commands
for year in $(seq $start_year $end_year); do
    commands+=("nohup Rscript ../rsc/data_preparation/sample_predictors.R $year $biome > ../log/s${year}.out 2> ../log/s${year}.err")
done

# Use xargs to run the commands in parallel (3 at a time)
printf "%s\n" "${commands[@]}" | xargs -I {} -P 3 bash -c '{} && echo "Finished {}"'
