// Define the years for computation
var startYear = 2001;
var endYear = 2024;

// Define tiles (8 tiles: 2 lat × 4 lon)
var tiles = [
  {name: "NWW", bounds: [-180, 0, -90, 90]}, // Northern West-West
  {name: "NW",  bounds: [-90, 0, 0, 90]},    // Northern West
  {name: "NE",  bounds: [0, 0, 90, 90]},     // Northern East
  {name: "NEE", bounds: [90, 0, 180, 90]},   // Northern East-East
  {name: "SWW", bounds: [-180, -90, -90, 0]},// Southern West-West
  {name: "SW",  bounds: [-90, -90, 0, 0]},   // Southern West
  {name: "SE",  bounds: [0, -90, 90, 0]},    // Southern East
  {name: "SEE", bounds: [90, -90, 180, 0]}   // Southern East-East
];

// Function to compute annual sum of NDVI and export for each tile
var computeAndExportTile = function(year, tile) {
  var yearStr = ee.String(ee.Number(year).format('%d'));
  
  // Filter MODIS NDVI collection for the year
  var ndviCollection = ee.ImageCollection('MODIS/061/MOD13A3')
    .filterDate(ee.Date.fromYMD(year, 1, 1), ee.Date.fromYMD(year, 12, 31))
    .select('NDVI');
  
  // Sum all monthly NDVI images for the year and convert to float
  var annualSum = ndviCollection
    .sum()
    .toFloat() // Fix for 'Type<Long>' error
    .rename('annual_ndvi_sum')
    .set('year', year)
    .set('tile', tile.name);

  // Define tile geometry
  var region = ee.Geometry.Rectangle(tile.bounds);

  // Export the tile to Google Drive
  Export.image.toDrive({
    image: annualSum,
    description: 'MODIS_Annual_NDVI_Sum_' + yearStr.getInfo() + '_' + tile.name,
    folder: 'MODIS_Annual_NDVI_Sums',
    fileNamePrefix: 'MODIS_Annual_NDVI_Sum_' + yearStr.getInfo() + '_' + tile.name,
    region: region,
    scale: 500, // Native MODIS resolution
    crs: 'EPSG:4326', // Native MODIS projection
    maxPixels: 1e13
  });

  print('Exporting tile:', tile.name, 'for year:', yearStr);
};

// Loop through years and tiles to export
for (var year = startYear; year <= endYear; year++) {
  tiles.forEach(function(tile) {
    computeAndExportTile(year, tile);
  });
}

print('All annual NDVI sum tiles are being exported.');