import os, sys
import pandas as pd
import requests
import tempfile
import h5py
import xarray as xr
import rioxarray
from rioxarray.merge import merge_arrays

useragent = "tis/download.py_1.0--" + sys.version.replace("\n", "").replace(
    "\r", ""
    )

with open("../prv/earthdata.token", "r") as f:
    token = f.read().replace("\n", "")

dir_lud = "L:" if sys.platform == "win32" else "/lud11"
f_urls = os.path.join("..", "misc", "VNP46A4", "VNP46A4.csv")
df_urls = pd.read_csv(f_urls)
df_urls["year"] = df_urls["fileURL"].apply(lambda x: x.split(".")[-5][1:5])

unique_years = df_urls["year"].unique()

headers = {
    "user-agent": useragent,
    "Authorization": "Bearer " + token
}

for year in unique_years:
    datasets = []
    urls = df_urls[df_urls["year"] == year]["fileURL"].values
    with tempfile.TemporaryDirectory() as folder_dl_dst:
        for url in urls:
            f_basename = os.path.basename(
                url
                ).replace(".", "_").replace("_h5", ".h5")
            
            dst = os.path.join(folder_dl_dst, f_basename)
            
            response = requests.get(url, stream = True, headers = headers)
            
            with open(dst, "wb") as file:
                for chunk in response.iter_content(chunk_size = 1024):
                    file.write(chunk)
            
            ds = xr.open_dataset(dst, engine = "rasterio")
            ds.rio.write_crs("EPSG:4326", inplace = True)
            datasets.append(var)
        
        merged_data = merge_arrays(datasets)
        var = merged_data["AllAngle_Composite_Snow_Free"]
        var.rio.to_raster(
            os.path.join(
                dir_lud, "poppman", "data", "bff", "dat", "nightlight_raw",
                f"Nightlight_{year}.tif"
                )
            )
        
        print(f"Year {year} downloaded.")