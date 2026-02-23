// Years
var startYear = 2018;
var endYear = 2025;

// Tiles (2 lat × 4 lon)
var tiles = [
  {name: "NWW", bounds: [-180, 0, -90, 90]},
  {name: "NW",  bounds: [-90, 0, 0, 90]},
  {name: "NE",  bounds: [0, 0, 90, 90]},
  {name: "NEE", bounds: [90, 0, 180, 90]},
  {name: "SWW", bounds: [-180, -90, -90, 0]},
  {name: "SW",  bounds: [-90, -90, 0, 0]},
  {name: "SE",  bounds: [0, -90, 90, 0]},
  {name: "SEE", bounds: [90, -90, 180, 0]}
];

// Annual MODIS NPP collection (yearly product)
var nppCollection = ee.ImageCollection("MODIS/061/MOD17A3HGF")
  .select("Npp");

// Years as list
var years = ee.List.sequence(startYear, endYear);

// Export function
var exportTile = function(year, tile) {
  var yearNum = ee.Number(year);
  var yearStr = yearNum.format("%d");
  // yearly image
  var annualNPP = nppCollection
    .filterDate(
      ee.Date.fromYMD(yearNum, 1, 1),
      ee.Date.fromYMD(yearNum, 12, 31)
    )
    .first()
    .toFloat()
    .rename("annual_npp")
    .set("year", yearNum)
    .set("tile", tile.name);

  var region = ee.Geometry.Rectangle({
    coords: tile.bounds,
    proj: "EPSG:4326",
    geodesic: false
  });

  Export.image.toDrive({
    image: annualNPP,
    description: "MODIS_Annual_NPP_" +
      yearStr.getInfo() + "_" + tile.name,
    folder: "MODIS_Annual_NPP",
    fileNamePrefix: "MODIS_Annual_NPP_" +
      yearStr.getInfo() + "_" + tile.name,
    region: region,
    scale: 500,
    crs: "EPSG:4326",
    maxPixels: 1e13
  });
  print("Exporting:", tile.name, yearStr);
};

// Run exports
years.getInfo().forEach(function(year) {
  tiles.forEach(function(tile) {
    exportTile(year, tile);
  });
});

print("All annual NPP tiles queued.");