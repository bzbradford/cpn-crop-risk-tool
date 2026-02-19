//--- Leaflet Cropscape integration ---//

// Function to create CDL layers
function createCDLLayers(map, years, options) {
  options = options || {};
  var defaultOpacity = options.opacity || 0.8;
  var autoAdd = options.autoAdd !== false;
  var attribution = options.attribution || 'USDA NASS Cropland Data Layer';

  var layerControl = map.currentLayersControl;

  // Create CDL GridLayer constructor
  var CDLLayer = L.GridLayer.extend({
    initialize: function (year, options) {
      this.year = year;
      L.GridLayer.prototype.initialize.call(this, options);
    },

    createTile: function (coords, done) {
      var tile = L.DomUtil.create('img', 'leaflet-tile');
      tile.width = 256;
      tile.height = 256;
      var nwPoint = coords.multiplyBy(256);
      var sePoint = nwPoint.add([256, 256]);
      var nw = map.unproject(nwPoint, coords.z);
      var se = map.unproject(sePoint, coords.z);
      var west = Math.max(-180, Math.min(nw.lng, se.lng));
      var east = Math.min(180, Math.max(nw.lng, se.lng));
      var south = Math.max(-85, Math.min(nw.lat, se.lat));
      var north = Math.min(85, Math.max(nw.lat, se.lat));
      var bbox = [west, south, east, north].join(',');
      var url = 'https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?' +
        'SERVICE=WMS&REQUEST=GetMap&VERSION=1.1.1' +
        '&LAYERS=cdl_' + this.year + '&SRS=EPSG:4326&BBOX=' + bbox +
        '&WIDTH=256&HEIGHT=256&FORMAT=image/png&TRANSPARENT=true';
      tile.onload = function () { done(null, tile); };
      tile.onerror = function () { done(new Error('Tile failed'), tile); };
      tile.src = url;
      return tile;
    }
  });

  // Create layers for each year
  years.forEach(function (year) {
    var cdlLayer = new CDLLayer(year, {
      attribution: attribution + ' ' + year,
      opacity: defaultOpacity
    });

    layerControl.addOverlay(cdlLayer, "Cropland Data Layer " + year);

    if (autoAdd && year === Math.max.apply(Math, years)) {
      cdlLayer.addTo(map);
    }
  });
}

// Initialize CDL layers when the map is defined
// 'map' global variable is set by a callback in the R leaflet call
function initializeCDL(years, options) {
  function checkForMap() {
    if (typeof map !== 'undefined' && map) {
      createCDLLayers(map, years, options);
    } else {
      setTimeout(checkForMap, 100);
    }
  }
  checkForMap();
}

// Auto-initialize when page loads
$(document).ready(function () {
  setTimeout(function () {
    initializeCDL([2024, 2023, 2022], { opacity: 0.8, autoAdd: false });
  }, 1000);
});
