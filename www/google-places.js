//--- Google Maps & Places Integration ---//

// Lat/lng bounds of the US and Canada
const BOUNDS = {
  south: 17.5,
  north: 71.5,
  west: -170.0,
  east: -52.5,
};

// Format address components into "City, State" format
function formatLocationName(addressComponents, fallbackName = '') {
  const locality = addressComponents.find((c) => c.types.includes('locality'));
  const state = addressComponents.find((c) => c.types.includes('administrative_area_level_1'));
  const city = locality ? locality.long_name : fallbackName;
  const stateAbbr = state ? state.short_name : '';

  return stateAbbr ? `${city}, ${stateAbbr}` : city;
}

// Geocoding API, called from server not client
// Not affected by Places => Places (New) api changes
// get locality name from coordinates
function getLocalityName(lat, lng, name, apiKey) {
  const url = `https://maps.googleapis.com/maps/api/geocode/json?latlng=${lat},${lng}&key=${apiKey}`;
  const inputId = 'map-locality_name';
  const loc = { lat: lat, lng: lng, name: name };

  fetch(url)
    .then((response) => response.json())
    .then((data) => {
      const addressComponents = data.results[0].address_components;
      loc.name = formatLocationName(addressComponents, name);
      console.debug('Parsed location as:', loc);
      sendShiny(inputId, loc);
    })
    .catch((error) => {
      console.error('Error:', error);
      sendShiny(inputId, loc);
    });
}

// Set up the places autocomplete on message from Shiny
Shiny.addCustomMessageHandler('google-places-init', async function (config) {
  bootstrapGoogleMaps(config.apiKey);
  setupGooglePlaces(config);
});

// load google places
// https://developers.google.com/maps/documentation/javascript/load-maps-js-api
function bootstrapGoogleMaps(apiKey) {
  ((g) => {
    var h,
      a,
      k,
      p = 'The Google Maps JavaScript API',
      c = 'google',
      l = 'importLibrary',
      q = '__ib__',
      m = document,
      b = window;
    b = b[c] || (b[c] = {});
    var d = b.maps || (b.maps = {}),
      r = new Set(),
      e = new URLSearchParams(),
      u = () =>
        h ||
        (h = new Promise(async (f, n) => {
          await (a = m.createElement('script'));
          e.set('libraries', [...r] + '');
          for (k in g)
            e.set(
              k.replace(/[A-Z]/g, (t) => '_' + t[0].toLowerCase()),
              g[k]
            );
          e.set('callback', c + '.maps.' + q);
          a.src = `https://maps.${c}apis.com/maps/api/js?` + e;
          d[q] = f;
          a.onerror = () => (h = n(Error(p + ' could not load.')));
          a.nonce = m.querySelector('script[nonce]')?.nonce || '';
          m.head.append(a);
        }));
    d[l]
      ? console.warn(p + ' only loads once. Ignoring:', g)
      : (d[l] = (f, ...n) => r.add(f) && u().then(() => d[l](f, ...n)));
  })({
    key: apiKey,
    v: 'weekly',
  });
}

// load library, wait for input element, bind to element
async function setupGooglePlaces(config) {
  try {
    // Autocomplete: the old API
    // PlaceAutocompleteElement: the new API
    const { Autocomplete, PlaceAutocompleteElement } = await google.maps.importLibrary('places');
    const inputElement = await waitForElement('#' + config.inputId);
    initPlacesAutocomplete(inputElement, config.outputId, Autocomplete);
    // initNewPlacesAutocomplete(inputElement, config.outputId, PlaceAutocompleteElement);
  } catch (error) {
    console.error('[Places] Initialization Error:', error);
  }
}

// using the old places API, bind to input element
function initPlacesAutocomplete(inputElement, outputId, AutocompleteClass) {
  const autocomplete = new AutocompleteClass(inputElement, {
    bounds: BOUNDS,
    strictBounds: false,
    componentRestrictions: { country: ['us', 'ca'] },
    fields: ['address_components', 'geometry', 'name'],
  });

  autocomplete.addListener('place_changed', () => {
    const place = autocomplete.getPlace();

    if (!place.geometry || !place.geometry.location) {
      console.warn("No details available for input: '" + place.name + "'");
      return;
    }

    const loc = place.geometry.location;
    const name = place.address_components
      ? formatLocationName(place.address_components, place.name)
      : place.name;

    // Send the data back to R
    Shiny.setInputValue(outputId, {
      name: name,
      lat: loc.lat(),
      lng: loc.lng(),
    });
  });

  // Prevent Shiny form submission on Enter
  inputElement.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
    }
  });
}

// use the new autocomplete API. On hold for now due to styling problems
function initNewPlacesAutocomplete(inputElement, outputId, AutocompleteClass) {
  // Initialize the new Web Component with updated property names
  const autocomplete = new AutocompleteClass({
    locationBias: BOUNDS, // Replaces bounds & strictBounds: false
    includedRegionCodes: ['us', 'ca'], // Replaces componentRestrictions
  });

  // Replace the existing Shiny input with the new Web Component
  // We transfer the ID so Shiny's UI layout remains intact if it relies on it.
  autocomplete.id = inputElement.id;
  inputElement.replaceWith(autocomplete);

  // Listen to the new 'gmp-select' event
  autocomplete.addEventListener('gmp-select', async (event) => {
    // Get the place prediction and fetch the required fields asynchronously
    const place = event.placePrediction.toPlace();

    await place.fetchFields({
      fields: ['displayName', 'location', 'addressComponents'],
    });

    if (!place.location) {
      console.warn("No details available for input: '" + place.displayName + "'");
      return;
    }

    // Map the new addressComponents format back to the old format to keep formatLocationName and Geocoder compatibility intact.
    const mappedComponents = place.addressComponents
      ? place.addressComponents.map((c) => ({
          types: c.types,
          long_name: c.longText,
          short_name: c.shortText,
        }))
      : null;

    const name = mappedComponents
      ? formatLocationName(mappedComponents, place.displayName)
      : place.displayName;

    // Send the data back to R
    Shiny.setInputValue(outputId, {
      name: name,
      lat: place.location.lat(),
      lng: place.location.lng(),
    });
  });

  // Prevent Shiny form submission on Enter
  autocomplete.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
    }
  });
}
