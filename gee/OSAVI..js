// Define the time period for the analysis
var startYear = 2016;
var endYear = 2018;
var startDate = startYear + '-01-01';
var endDate = endYear + '-12-31';

// Define the MODIS product (MOD09A1: Surface Reflectance 500m).
var modis = ee.ImageCollection(
  'MODIS/006/MOD09A1'
).filterDate(startDate, endDate).select(
  ['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03']
); // Red, Green, and NIR bands (bands 1, 2, and 3)

// Define a function to calculate OSAVI
var calculateOSAVI = function(image) {
  var scaleFactor = 0.0001;
  var red = image.select('sur_refl_b01').multiply(scaleFactor);; // Red band
  var nir = image.select('sur_refl_b02').multiply(scaleFactor);; // NIR band
  var L = 0.16; // Optimized soil adjustment factor for OSAVI
  
  // Calculate OSAVI: (NIR - RED) / (NIR + RED + L) * (1 + L)
  var osavi = nir.subtract(
    red
  ).divide(
    nir.add(red).add(L)
  ).multiply(1 + L).rename(
    'OSAVI'
  );
  
  return image.addBands(osavi);
};

// Apply the OSAVI function to the MODIS collection
var modisWithOSAVI = modis.map(calculateOSAVI);

// Function to compute the maximum OSAVI value for each month
var monthlyMaxOSAVI = function(year) {
  var start = ee.Date.fromYMD(year, 1, 1);
  var end = start.advance(1, 'year');
  
  var yearCollection = modisWithOSAVI
    .filterDate(start, end)
    .map(function(image) {
      var month = image.date().get('month');
      return image.set('month', month);
    });
  
  var monthlyMax = ee.List.sequence(1, 12).map(function(month) {
    var monthlyImages = yearCollection.filter(ee.Filter.eq('month', month));
    
    // Check if there are any images for this month
    var count = monthlyImages.size();
    
    // If there are no images, return a constant empty image
    return ee.Algorithms.If(
      count.gt(0),
      ee.ImageCollection(monthlyImages).max().select('OSAVI'),
      ee.Image().rename('OSAVI').set('empty', true) // Placeholder empty image
    );
  });
  
  return ee.ImageCollection(monthlyMax).toBands().set('system:time_start', start.millis());
};

// Generate the annual average OSAVI for each year
var annualOSAVI = ee.List.sequence(startYear, endYear).map(function(year) {
  var maxMonthlyOSAVI = monthlyMaxOSAVI(year);
  
  // Calculate the mean for the entire year
  var annualImage = maxMonthlyOSAVI.reduce(ee.Reducer.mean())
    .set('year', year)
    .rename('Annual_OSAVI');
  
  return annualImage;
});

// Convert the list to an image collection
var annualOSAVICollection = ee.ImageCollection(annualOSAVI);

// Define tiles (8 tiles: 2 lat × 4 lon) with adjusted latitude ranges
var tiles = [
  {name: "NWW", bounds: [-180, 0, -90, 60]}, // Northern West-West
  {name: "NW",  bounds: [-90, 0, 0, 60]},    // Northern West
  {name: "NE",  bounds: [0, 0, 90, 60]},     // Northern East
  {name: "NEE", bounds: [90, 0, 180, 60]},   // Northern East-East
  {name: "SWW", bounds: [-180, -60, -90, 0]},// Southern West-West
  {name: "SW",  bounds: [-90, -60, 0, 0]},   // Southern West
  {name: "SE",  bounds: [0, -60, 90, 0]},    // Southern East
  {name: "SEE", bounds: [90, -60, 180, 0]}   // Southern East-East
];

// Loop through all years and tiles and export each result
ee.List.sequence(startYear, endYear).getInfo().forEach(function(year) {
  // Filter the annual OSAVI for the current year
  var selectedYear = annualOSAVICollection.filter(ee.Filter.eq('year', year)).first();
  
  // Loop through each tile for export
  tiles.forEach(function(tile) {
    var tileBounds = ee.Geometry.Rectangle(tile.bounds);
    
    // Clip the image to the current tile bounds
    var clippedImage = selectedYear.clip(tileBounds);
    
    Map.centerObject(clippedImage, 6);  // Zoom to the area of the first image
    Map.addLayer(clippedImage, {min: 0, max: 0.5, palette: ['blue', 'green', 'yellow', 'red']}, 'First OSAVI Image');

    // Export the clipped image for the current tile and year
    Export.image.toDrive({
      image: clippedImage,
      description: 'Annual_OSAVI_' + year + '_' + tile.name,
      folder: 'MODIS_Annual_OSAVI_Means',
      scale: 500,
      crs: 'EPSG:4326',
      region: tileBounds,
      fileFormat: 'GeoTIFF',
      maxPixels: 1e13
    });
  });
});
