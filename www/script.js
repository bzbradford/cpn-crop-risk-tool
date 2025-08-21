// JS functions for CPN Crop Risk Tool

// hide bootstrap deprecation warnings (valid as of 2025-08-21)
const originalWarn = console.warn;
console.warn = function(message, ...args) {
  const stack = new Error().stack;
  if (stack && stack.includes('bootstrap-datepicker.min.js')) {
    return; // Suppress warnings from this specific file
  }
  originalWarn.apply(console, [message, ...args]);
}


// make all modal links open in new tab
$(document).on("shown.bs.modal", function () {
  $(".modal a").attr("target", "_blank");
});

// helper to send values to the shiny server
function sendShiny(inputId, content) {
  Shiny.setInputValue(inputId, content, { priority: "event" });
}

//--- Google Places integration ---//

// Lat/lng bounds of the continental US
const BOUNDS = {
  south: 24.5,
  north: 49.0,
  west: -125.0,
  east: -66.9,
};

// Format address components into "City, State" format
function formatLocationName(addressComponents, fallbackName = "") {
  const locality = addressComponents.find((component) => component.types.includes("locality"));
  const state = addressComponents.find((component) =>
    component.types.includes("administrative_area_level_1")
  );

  const city = locality ? locality.long_name : fallbackName;
  const stateAbbr = state ? state.short_name : "";

  return stateAbbr ? `${city}, ${stateAbbr}` : city;
}

// callback for google location searchbox
function initAutocomplete() {
  const searchbox = document.getElementById("map-searchbox");
  const inputId = "map-searched_loc";
  const opts = {
    types: ["geocode"],
    bounds: BOUNDS,
    strictBounds: true,
  };

  const autocomplete = new google.maps.places.Autocomplete(searchbox, opts);
  autocomplete.setFields(["name", "geometry", "address_components"]);
  autocomplete.addListener("place_changed", function () {
    const place = autocomplete.getPlace();
    // console.log(place)
    if (!place.geometry) return;
    const loc = place.geometry.location;

    // Format name like getLocalityName function: City, State Abbreviation
    const formattedName = place.address_components
      ? formatLocationName(place.address_components, place.name)
      : place.name;

    const response = { name: formattedName, lat: loc.lat(), lng: loc.lng() };
    sendShiny(inputId, response);
  });
}

// get locality name from coordinates
function getLocalityName(lat, lng, name, apiKey) {
  const url = `https://maps.googleapis.com/maps/api/geocode/json?latlng=${lat},${lng}&key=${apiKey}`;
  const inputId = "map-locality_name";
  const loc = { lat: lat, lng: lng, name: name };

  fetch(url)
    .then((response) => response.json())
    .then((data) => {
      const addressComponents = data.results[0].address_components;
      loc.name = formatLocationName(addressComponents, name);
      // console.log('Parsed location as:', loc)
      sendShiny(inputId, loc);
    })
    .catch((error) => {
      console.error("Error:", error);
      sendShiny(inputId, loc);
    });
}

//--- Cookie handling with User ID ---//

const COOKIE_NAME = "ibm_weather_tool";

// Generate a unique user ID
function generateUserId() {
  // Generate a cryptographically secure UUID v4
  // https://stackoverflow.com/a/2117523/1422451
  return ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, (c) =>
    (c ^ (crypto.getRandomValues(new Uint8Array(1))[0] & (15 >> (c / 4)))).toString(16)
  );
}

// Get or create user data object
function getUserData() {
  let cookieValue = getCookie();
  let userData;

  try {
    userData = cookieValue ? JSON.parse(cookieValue) : {};
  } catch (e) {
    console.warn("Invalid cookie data, resetting:", e);
    userData = {};
  }

  // Check if required keys exist, if not reset the cookie
  if (!userData.userId || !userData.sites) {
    console.warn("Missing required cookie keys, resetting user data");
    userData = {
      userId: generateUserId(),
      sites: [],
    };
    saveUserData(userData);
  }

  return userData;
}

// Save user data to cookie
function saveUserData(userData) {
  setCookie(userData);
  return userData;
}

// Update sites and save
function updateSites(newSites) {
  const userData = getUserData();
  userData.sites = newSites;
  saveUserData(userData);
  return userData;
}

function setCookie(value, name = COOKIE_NAME, days = 30) {
  let expires = "";
  if (days) {
    let date = new Date();
    date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
    expires = "; expires=" + date.toUTCString();
  }
  if (typeof value === "object") {
    value = JSON.stringify(value);
  }
  document.cookie = name + "=" + value + expires + "; path=/";
  return name;
}

function getCookie(name = COOKIE_NAME) {
  let nameEQ = name + "=";
  let ca = document.cookie.split(";");
  for (let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == " ") c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}

function sendCookieToShiny(name = COOKIE_NAME) {
  let userData = getUserData();
  sendShiny("cookie", userData);
  return userData;
}

function deleteCookie(name = COOKIE_NAME) {
  document.cookie = name + "=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;";
  return name;
}

//--- Site action buttons ---//

function editSite(site_id, site_name = "") {
  let newName = prompt(`Enter a new name for site ${site_id}:`, site_name);
  if (newName) sendShiny("edit_site", { id: site_id, name: newName });
}

function trashSite(site_id) {
  if (confirm(`Remove site ${site_id}?`)) sendShiny("trash_site", site_id);
}

function saveSite(site_id) {
  sendShiny("save_site", site_id);
}

function clearSites() {
  if (confirm("Remove all sites?")) sendShiny("clear_sites", true);
}


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
    initialize: function(year, options) {
      this.year = year;
      L.GridLayer.prototype.initialize.call(this, options);
    },
    
    createTile: function(coords, done) {
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
      tile.onload = function() { done(null, tile); };
      tile.onerror = function() { done(new Error('Tile failed'), tile); };
      tile.src = url;
      return tile;
    }
  });
  
  // Create layers for each year
  years.forEach(function(year) {
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
$(document).ready(function() {
  setTimeout(function() {
    initializeCDL([2024, 2023, 2022], {opacity: 0.8, autoAdd: false});
  }, 1000);
});

