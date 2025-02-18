// Load the MODIS Land Cover Type dataset (MCD12Q1)
var landCover = ee.ImageCollection('MODIS/006/MCD12Q1')
                .filter(ee.Filter.date('2002-01-01', '2002-12-31'))
                .first();

// Select the University of Maryland (UMD) layer
var pft = landCover.select('LC_Type2');

// Define the binary classification: Deserts (classes 7 and 15)
var desertBinary = pft.expression(
    '(LC == 7) || (LC == 15) ? 1 : 0',
    {
      'LC': pft
    }
).updateMask(pft.eq(7).or(pft.eq(15)));

// Add the binary classification layer to the map
Map.centerObject(landCover, 3);

var targetCRS = 'EPSG:4326';

// Reproject the image to the target CRS
var reprojectedBinary = desertBinary.reproject({
  crs: targetCRS,
  scale: 500
}).uint8();

Map.addLayer(desertBinary, {
  min: 0, max: 1, palette: ['gray', 'yellow']
}, 'Desert Binary Classification');

// Export the binary classification map to Google Drive
var ExportRegion = ee.Geometry.Rectangle([-180, -90, 0, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'DesertBinaryClassification',
  folder: 'EarthEngineExports',
  fileNamePrefix: 'desert_binary',
  scale: 500,
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13
});

var ExportRegion = ee.Geometry.Rectangle([0, -90, 180, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'DesertBinaryClassification',
  folder: 'EarthEngineExports',
  fileNamePrefix: 'desert_binary',
  scale: 500,
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13
});