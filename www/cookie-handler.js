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

// get data from cookie
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

// saves data to the cookie
function setCookie(data, name = COOKIE_NAME, days = 30) {
  let expires = "";
  if (days) {
    let date = new Date();
    date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
    expires = "; expires=" + date.toUTCString();
  }
  if (typeof data === "object") {
    data = JSON.stringify(data);
  }
  document.cookie = name + "=" + data + expires + "; path=/";
  return name;
}

// Update cookie with a partial data object (merges into existing userData)
function updateCookie(updates) {
  const userData = getUserData();
  Object.assign(userData, updates);
  setCookie(userData);
  return userData;
}

// clears cookie by setting expiration to a past date
function deleteCookie(name = COOKIE_NAME) {
  document.cookie = name + "=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;";
  return name;
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
    setCookie(userData);
  }

  return userData;
}

// send data to Shiny input$cookie
function sendCookieToShiny() {
  let userData = getUserData();
  sendShiny("cookie", userData);
  return userData;
}
