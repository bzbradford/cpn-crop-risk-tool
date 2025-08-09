// JS functions for CPN Crop Risk Tool

// make all modal links open in new tab
$(document).on('shown.bs.modal', function() {
  $('.modal a').attr('target', '_blank');
});

// helper to send values to the shiny server
function sendShiny(inputId, content) {
  Shiny.setInputValue(inputId, content, { priority: 'event'} )
}

//--- Google Places integration ---//

// Lat/lng bounds of the continental US
const BOUNDS = {
  south: 24.5, north: 49.0,
  west: -125.0, east: -66.9,
}

// callback for google location searchbox
function initAutocomplete() {
  const searchbox = document.getElementById('map-searchbox');
  const inputId = 'map-searched_loc'
  const opts = {
    types: ['geocode'],
    bounds: BOUNDS,
    strictBounds: true,
  }

  autocomplete = new google.maps.places.Autocomplete(searchbox, opts);
  autocomplete.setFields(['name', 'geometry']);
  autocomplete.addListener('place_changed', function() {
    const place = autocomplete.getPlace();
    // console.log(place)
    if (!place.geometry) return;
    const loc = place.geometry.location;
    const response = { name: place.name, lat: loc.lat(), lng: loc.lng() }
    sendShiny(inputId, response);
  });
}

// get locality name from coordinates
function getLocalityName(lat, lng, name, apiKey) {
  const url = `https://maps.googleapis.com/maps/api/geocode/json?latlng=${lat},${lng}&key=${apiKey}`;
  const inputId = 'map-locality_name'
  const loc = { lat: lat, lng: lng, name: name }

  fetch(url)
    .then(response => response.json())
    .then(data => {
      const addressComponents = data.results[0].address_components;
      const locality = addressComponents.find(component => 
        component.types.includes('locality')
      );
      const city = locality ? locality.long_name : name;
      loc.name = city
      // console.log('Parsed location as:', loc)
      sendShiny(inputId, loc)
    })
    .catch(error => {
      console.error('Error:', error)
      sendShiny(inputId, loc)
    });
}


//--- Cookie handling with User ID ---//

const COOKIE_NAME = 'ibm_weather_tool'

// Generate a unique user ID
function generateUserId() {
  // Generate a cryptographically secure UUID v4
  // https://stackoverflow.com/a/2117523/1422451
  return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}

// Get or create user data object
function getUserData() {
  let cookieValue = getCookie();
  let userData;
  
  try {
    userData = cookieValue ? JSON.parse(cookieValue) : {};
  } catch (e) {
    console.warn('Invalid cookie data, resetting:', e);
    userData = {};
  }
  
  // Ensure user has an ID
  if (!userData.userId) {
    userData.userId = generateUserId();
  }
  
  // Ensure sites array exists
  if (!userData.sites) {
    userData.sites = [];
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
  let expires = '';
  if (days) {
    let date = new Date();
    date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
    expires = '; expires=' + date.toUTCString();
  }
  if (typeof value === 'object') {
    value = JSON.stringify(value);
  }
  document.cookie = name + '=' + value + expires + '; path=/';
  return name;
}

function getCookie(name = COOKIE_NAME) {
  let nameEQ = name + '=';
  let ca = document.cookie.split(';');
  for(let i = 0; i < ca.length; i++) {
    let c = ca[i];
    while (c.charAt(0) == ' ') c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}

function sendCookieToShiny(name = COOKIE_NAME) {
  let userData = getUserData();
  sendShiny('cookie', userData);
  return userData;
}

function deleteCookie(name = COOKIE_NAME) {
  document.cookie = name + '=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
  return name;
}


//--- Site action buttons ---//

function editSite(site_id, site_name = '') {
  let newName = prompt(`Enter a new name for site ${site_id}:`, site_name);
  if (newName) sendShiny('edit_site', { id: site_id, name: newName } );
}

function trashSite(site_id) {
  if (confirm(`Remove site ${site_id}?`)) sendShiny('trash_site', site_id);
}

function saveSite(site_id) {
  sendShiny('save_site', site_id);
}

function clearSites() {
  if (confirm('Remove all sites?')) sendShiny('clear_sites', true);
}
