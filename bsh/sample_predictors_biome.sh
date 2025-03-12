start_year=2002
end_year=2018
biome=${1:-""}

# Create an array to hold the commands
commands=""

# Generate the commands
for year in $(seq $start_year $end_year); do
    commands+="Rscript ../rsc/data_preparation/sample_predictors.R $year $biome ; "
done

commands+="echo 'Finished.'"

# Log file paths with the biome variable
log_folder="../log"
output_log="${log_folder}/b${biome}.log"
error_log="${log_folder}/b${biome}.log"

nohup bash -c "$commands" > "$output_log" 2> "$error_log" &
