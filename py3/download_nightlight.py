import os, sys
import logging
import pandas as pd
import requests
import tempfile
import h5py
import xarray as xr
import rioxarray
from rioxarray.merge import merge_arrays
from tqdm import tqdm
from contextlib import redirect_stdout, redirect_stderr

logging.getLogger("requests").setLevel(logging.CRITICAL)
logging.getLogger("rasterio").setLevel(logging.CRITICAL)
logging.getLogger("xarray").setLevel(logging.CRITICAL)

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

for year in tqdm(unique_years, desc = "Processing Years", leave = True):
    datasets = []
    urls = df_urls[df_urls["year"] == year]["fileURL"].values
    with tempfile.TemporaryDirectory() as folder_dl_dst:
        for url in urls:
            f_basename = os.path.basename(
                url
                ).replace(".", "_").replace("_h5", ".h5")
            
            dst = os.path.join(folder_dl_dst, f_basename)
            
            response = requests.get(url, stream = True, headers = headers)
            total_size = int(response.headers.get("content-length", 0))
            
            with open(os.devnull, "w") as devnull:
                with open(dst, "wb") as file, redirect_stdout(devnull):
                    with tqdm(
                        desc = f"Downloading {f_basename}",
                        total = total_size,
                        unit = "B",
                        unit_scale = True,
                        unit_divisor = 1024,
                        leave = False,
                        disable = True
                    ) as pbar:
                        for chunk in response.iter_content(chunk_size = 1024):
                            file.write(chunk)
                            pbar.update(len(chunk))

                try:
                    with redirect_stdout(devnull), redirect_stderr(devnull):
                        ds = xr.open_dataset(dst, engine = "rasterio")
                        ds.rio.write_crs("EPSG:4326", inplace = True)
                        datasets.append(ds)
                except Exception as e:
                    print(f"Error processing {dst}: {e}")
        
        with open(os.devnull, "w") as devnull:
            with redirect_stdout(devnull), tqdm(
                desc = f"Merging {year}", total = len(datasets), leave = False
                ) as pbar:
                merged_data = merge_arrays(datasets)
                var = merged_data["AllAngle_Composite_Snow_Free"]
                var.rio.to_raster(
                    os.path.join(
                        dir_lud,
                        "poppman", "data", "bff", "dat", "nightlight_raw",
                        f"Nightlight_{year}.tif"
                        )
                    )
    pbar.update(len(datasets))
    pbar.close()
