//--- Leaflet Cropscape integration ---//

// Function to create CDL layers
function createCDLLayers(map, years) {
  const layerOpacity = 0.8;
  const attribution = 'USDA NASS Cropland Data Layer';
  const layerControl = map.currentLayersControl;

  // Create CDL GridLayer constructor
  const CDLLayer = L.GridLayer.extend({
    initialize: function (year, options) {
      this.year = year;
      L.GridLayer.prototype.initialize.call(this, options);
    },

    createTile: function (coords, done) {
      const tile = L.DomUtil.create('img', 'leafconst-tile');
      tile.width = 256;
      tile.height = 256;
      const nwPoint = coords.multiplyBy(256);
      const sePoint = nwPoint.add([256, 256]);
      const nw = map.unproject(nwPoint, coords.z);
      const se = map.unproject(sePoint, coords.z);
      const west = Math.max(-180, Math.min(nw.lng, se.lng));
      const east = Math.min(180, Math.max(nw.lng, se.lng));
      const south = Math.max(-85, Math.min(nw.lat, se.lat));
      const north = Math.min(85, Math.max(nw.lat, se.lat));
      const bbox = [west, south, east, north].join(',');
      const url =
        'https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?' +
        'SERVICE=WMS&REQUEST=GetMap&VERSION=1.1.1' +
        '&LAYERS=cdl_' +
        this.year +
        '&SRS=EPSG:4326&BBOX=' +
        bbox +
        '&WIDTH=256&HEIGHT=256&FORMAT=image/png&TRANSPARENT=true';
      tile.onload = function () {
        done(null, tile);
      };
      tile.onerror = function () {
        done(new Error('Tile failed'), tile);
      };
      tile.src = url;
      return tile;
    },
  });

  // Create layers for each year
  years.forEach(function (year) {
    const cdlLayer = new CDLLayer(year, {
      attribution: attribution + ' ' + year,
      opacity: layerOpacity,
    });
    layerControl.addOverlay(cdlLayer, 'Cropland Data Layer ' + year);
  });
}
