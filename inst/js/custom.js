// File is included into the HTML of the teal UI and should only contain functions
// init.js contains functions that should be executed from the Shiny server to the browser, e.g. using shinyjs


// unlike Doug, we don't create a Shiny input binding because we want to handle this entirely in the browser and not on the server
// wait for init, otherwise, the elements are not there yet
// Note: these functions don't work yet and a good place where to add them must be found (which event to register with?)
function convertToDraggable(el) {
  console.log('Converting element to draggable');
  Sortable.create(el, {
    // handle: ".sortableJS-handle", // only move element when dragging on handle element
    animation: 150,
    onEnd: function (evt) {
      $(el).trigger('end');
  }});
}
