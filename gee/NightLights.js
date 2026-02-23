var startYear = 2014;
var endYear = 2025;

var exportAnnualComposite = function(year) {
  var yearStr = ee.String(ee.Number(year).format("%d"));
  var collection = ee.ImageCollection(
    "NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG"
    ).filterDate(
      ee.Date.fromYMD(year, 1, 1), ee.Date.fromYMD(year, 12, 31)
      ).select(
        "avg_rad"
        ).map(function(img) {// Mask extreme values
        var mask = img.select("avg_rad").gt(0).and(img.select("avg_rad").lt(500));
        return img.updateMask(mask);
        }
        );

  // Compute annual mean composite
  var annualMean = collection.mean()
  
  // Export task
  Export.image.toDrive({
    image: annualMean,
    description: 'VIIRS_AnnualMeanE_' + yearStr.getInfo(),
    folder: 'GEE_VIIRS_Annual_Means',
    fileNamePrefix: 'VIIRS_AnnualMeanE_' + yearStr.getInfo(),
    region: ee.Geometry.Rectangle([0, -90, 180, 90]),
    scale: 500,
    maxPixels: 1e13,
    crs: 'EPSG:4326'
  });
  Export.image.toDrive({
    image: annualMean,
    description: 'VIIRS_AnnualMeanW_' + yearStr.getInfo(),
    folder: 'GEE_VIIRS_Annual_Means',
    fileNamePrefix: 'VIIRS_AnnualMeanW_' + yearStr.getInfo(),
    region: ee.Geometry.Rectangle([-180, -90, 0, 90]),
    scale: 500,
    maxPixels: 1e13,
    crs: 'EPSG:4326'
  });
};

// Loop over years
for (var year = startYear; year <= endYear; year++) {
  exportAnnualComposite(year);
  print("Exported ".concat(year))
}

print("All exports have been started.");