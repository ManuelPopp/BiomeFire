#!/bin/bash
src_dir="/lud11/poppman/data/bff/dat/lud11/annual/npp_biome4_MODIS"
dst_dir="/lud11/poppman/data/bff/dat/lud11/annual/npp_biome4_MODISty"
mkdir -p "$dst_dir"

for file in "$src_dir"/npp_biome4_before_*.tif; do
  year=$(echo "$file" | grep -oE '[0-9]{4}')
  new_year=$((year - 1))
  new_file=$(echo "$file" | sed -E "s/_before_${year}/_${new_year}/")
  cp "$file" "$dst_dir/$(basename "$new_file")"
done