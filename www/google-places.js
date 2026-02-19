//--- Google Places integration ---//

// Lat/lng bounds of the US and Canada
const BOUNDS = {
  south: 17.5,
  north: 71.5,
  west: -170.0,
  east: -52.5,
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
