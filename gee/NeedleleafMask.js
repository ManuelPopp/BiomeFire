// Load the MODIS Land Cover Type dataset (MCD12Q1)
var landCover = ee.ImageCollection('MODIS/006/MCD12Q1')
                .filter(ee.Filter.date('2002-01-01', '2002-12-31'))
                .first();

// Select the Plant Functional Types (PFT) layer
var pft = landCover.select('LC_Type5');

// Define the binary classification: Needleleaf forests (classes 4 and 5)
var needleleafBinary = pft.expression(
    'LC == 1 ? 1 : 0',
    {
      'LC': pft
    }
).updateMask(pft.eq(1));

// Add the binary classification layer to the map
Map.centerObject(landCover, 3);

var targetCRS = 'EPSG:4326';

// Reproject the image to the target CRS
var reprojectedBinary = needleleafBinary.reproject({
  crs: targetCRS,
  scale: 500
}).uint8();

Map.addLayer(needleleafBinary, {
  min: 0, max: 1, palette: ['gray', 'green']
}, 'Needleleaf Binary Classification');

// Export the binary classification map to Google Drive
var ExportRegion = ee.Geometry.Rectangle([-180, -90, 0, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'NeedleleafBinaryClassification',
  folder: 'EarthEngineExports',
  fileNamePrefix: 'needleevergreen_binary1',
  scale: 500,
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13
});

var ExportRegion = ee.Geometry.Rectangle([0, -90, 180, 90]);
Export.image.toDrive({
  image: reprojectedBinary,
  description: 'NeedleleafBinaryClassification',
  folder: 'EarthEngineExports',
  fileNamePrefix: 'needleevergreen_binary2',
  scale: 500,
  crs: targetCRS,
  region: ExportRegion,
  maxPixels: 1e13
});