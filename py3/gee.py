#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 10:01:02 2024

Compare MODIS and LANDSAT monthly burned area products.

"""
__author__ = "Manuel"
__date__ = "Tue Nov 19 10:01:02 2024"
__credits__ = ["Manuel R. Popp"]
__license__ = "Unlicense"
__version__ = "0.0.1"
__maintainer__ = "Manuel R. Popp"
__email__ = "requests@cdpopp.de"
__status__ = "Production"

#-----------------------------------------------------------------------------|
# Imports
import os
import sys
import time
import warnings
from datetime import datetime as dt
from tqdm import tqdm
import pandas as pd
import geopandas as gpd

import requests

import ee
import geojson

#-----------------------------------------------------------------------------|
# Settings
dir_main = os.path.dirname(os.getcwd())

ee.Authenticate()
ee.Initialize(project = "ee-manuelpopp")

# MODIS Burned Area Monthly Global 500 m (Version 6.1)
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD64A1
MODIS_BURNED_AREA = "MODIS/061/MCD64A1"
MODIS_LANDCOVER = "MODIS/061/MCD12Q1"
aoi = ee.Geometry.BBox(-180, -90, 180, 90)

def export_image(image, index, aoi = aoi):
    date = image.date().format("YYYY-MM-dd").getInfo()
    
    image_geometry = image.geometry()
    if image_geometry.isUnbounded():
        print(f"Image {index + 1} has unbounded geometry. Using global AOI for clipping: {date}")
        clipped_geometry = aoi
    else:
        clipped_geometry = image_geometry.intersection(aoi, ee.ErrorMargin(1))
    
    standardized_image = image.toInt16()
    clipped_image = standardized_image.clip(
        image_geometry.intersection(clipped_geometry, ee.ErrorMargin(1))
        )
    
    task = ee.batch.Export.image.toDrive(
        image = clipped_image,
        description = f"MODIS_MCD64A1_{date}",
        folder = "GEE_Exports",
        fileNamePrefix = f"MODIS_MCD64A1_{date}",
        scale = 500,
        maxPixels = 10000000000000# This is the max allowed value.
    )
    task.start()

def process_year(year, collection, aoi = aoi):
    # Filter collection for the respective year
    year_collection = collection.filterDate(f"{year}-01-01", f"{year}-12-31")

    # Compute min FirstDay, max LastDay, and BurnDate binary summary
    min_first_day = year_collection.select("FirstDay").reduce(ee.Reducer.min())
    max_last_day = year_collection.select("LastDay").reduce(ee.Reducer.max())

    def compute_burned(image):
        return image.gt(0).toByte()
    
    burn_date_binary = year_collection.select(
        "BurnDate"
        ).map(compute_burned).reduce(ee.Reducer.anyNonZero())
    
    burned_first_doy = year_collection.select(
        "BurnDate"
        ).min().rename("BurnedFirstDOY")
    
    burned_last_doy = year_collection.select(
        "BurnDate"
        ).max().rename("BurnedLastDOY")

    # Combine the bands into a single image
    result = min_first_day.rename("FirstDay").addBands(
        max_last_day.rename("LastDay")
    ).addBands(
        burn_date_binary.rename("Burned").toInt16()
    ).addBands(
        burned_first_doy.toInt16()
    ).addBands(
        burned_last_doy.toInt16()
    )

    # Export the yearly result to Google Drive
    task = ee.batch.Export.image.toDrive(
        image = result.clip(aoi),
        description = f"MODIS_MCD64A1_Summary_{year}",
        folder = "GEE_Exports",
        fileNamePrefix = f"MODIS_MCD64A1_Summary_{year}",
        region = aoi,
        scale = 500,
        maxPixels = 10000000000000
    )
    task.start()
    print(f"Export task started for year: {year}")

def download_images(images):
    for i in range(images.size().getInfo()):
        export_image(ee.Image(images.get(i)), i)

def summarise_by_year(years, collection):
    for year in years:
        process_year(year, collection = collection)

#-----------------------------------------------------------------------------|
# Download MODIS data
# Authenticate and initialize the Earth Engine API
burned_area_collection = ee.ImageCollection(MODIS_BURNED_AREA)

images = burned_area_collection.toList(burned_area_collection.size())

years = list(range(2000, 2026))

summarise_by_year(years, collection = burned_area_collection)
print("All export tasks have been started.")
