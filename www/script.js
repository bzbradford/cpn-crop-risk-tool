// JS functions for CPN Crop Risk Tool

// hide bootstrap deprecation warnings (valid as of 2025-08-21)
const originalWarn = console.warn;
console.warn = function (message, ...args) {
  const stack = new Error().stack;
  if (stack && stack.includes('bootstrap-datepicker.min.js')) {
    return; // Suppress warnings from this specific file
  }
  originalWarn.apply(console, [message, ...args]);
};

// make all modal links open in new tab
$(document).on('shown.bs.modal', function () {
  $('.modal a').attr('target', '_blank');
});

// helper to send values to the shiny server
function sendShiny(inputId, content) {
  Shiny.setInputValue(inputId, content, { priority: 'event' });
}

//--- Utilities ---//

// Helper to force a timeout on an async operation
function promiseWithTimeout(promise, ms, timeoutMessage) {
  let timeoutHandle;
  const timeoutPromise = new Promise((_, reject) => {
    timeoutHandle = setTimeout(() => reject(new Error(timeoutMessage)), ms);
  });

  return Promise.race([promise, timeoutPromise]).then((result) => {
    clearTimeout(timeoutHandle); // Clean up the timer if the promise succeeds
    return result;
  });
}

// Returns a Promise that resolves when the element appears in the DOM
function waitForElement(selector) {
  return new Promise((resolve) => {
    // If it's already there, resolve immediately
    if (document.querySelector(selector)) {
      return resolve(document.querySelector(selector));
    }

    // Otherwise, set up an observer to watch for DOM changes
    const observer = new MutationObserver((mutations) => {
      if (document.querySelector(selector)) {
        observer.disconnect(); // Stop watching once we find it
        resolve(document.querySelector(selector));
      }
    });

    // Start observing the body for injected child nodes (like Shiny's renderUI)
    observer.observe(document.body, {
      childList: true,
      subtree: true,
    });
  });
}

//--- Site action buttons ---//

function editSite(site_id, site_name = '') {
  let newName = prompt(`Enter a new name for site ${site_id}:`, site_name);
  if (newName) sendShiny('edit_site', { id: site_id, name: newName });
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
