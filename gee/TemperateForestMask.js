// Load the MODIS Land Cover Type dataset (MCD12Q1)
var landCover = ee.ImageCollection('MODIS/006/MCD12Q1')
                .filter(ee.Filter.date('2002-01-01', '2002-12-31'))
                .first(); // Get the first image for the specified year

// Select the University of Maryland (UMD) layer
var pft = landCover.select('LC_Type2');

// Define the binary classification: Temperate forests (classes 4 and 5)
var tforestBinary = pft.expression(
    '(LC >= 4) && (LC <= 5) ? 1 : 0',
    {
      'LC': pft
    }
).updateMask(pft.gte(4).and(pft.lte(5)));

// Add the binary classification layer to the map
Map.centerObject(landCover, 3);

var targetCRS = 'EPSG:4326';

// Reproject the image to the target CRS
var reprojectedBinary = tforestBinary.reproject({
  crs: targetCRS,
  scale: 500 // Ensure the scale is appropriate for your target CRS
}).uint8();

Map.addLayer(tforestBinary, {
  min: 0, max: 1, palette: ['gray', 'green']
}, 'Temperate Binary Classification');

// Export the binary classification map to Google Drive
var ExportRegion = ee.Geometry.Rectangle([-180, -90, 0, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'TemperateForestBinaryClassification',
  folder: 'EarthEngineExports', // Optional: specify a folder in your Google Drive
  fileNamePrefix: 'temperateforest_binary',
  scale: 500, // Scale in meters (MODIS resolution is ~500m)
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13 // Adjust as needed for large exports
});

var ExportRegion = ee.Geometry.Rectangle([0, -90, 180, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'TemperateForestBinaryClassification',
  folder: 'EarthEngineExports', // Optional: specify a folder in your Google Drive
  fileNamePrefix: 'temperateforest_binary',
  scale: 500, // Scale in meters (MODIS resolution is ~500m)
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13 // Adjust as needed for large exports
});