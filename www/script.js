// JS functions for CPN Crop Risk Tool

// hide bootstrap deprecation warnings (valid as of 2025-08-21)
const originalWarn = console.warn;
console.warn = function (message, ...args) {
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


