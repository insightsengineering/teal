// This file contains functions that should be executed at the start of each session

var clipboard = new ClipboardJS('.btn[data-clipboard-target]');

// unlike Doug, we don't create a Shiny input binding because we want to handle this entirely in the browser and not on the server
// wait for init, otherwise, the elements are not there yet
$( document ).on("shiny:sessioninitialized", function(event) {
  debugger;
  var listsWithHandle = $(document).find(".listWithHandle");
  // show logs by calling `showLog()` in the server and `useShinyjs()` in the UI
  console.log("Found " + listsWithHandle.length + " lists to make draggable");
  for (var j = 0; j < listsWithHandle.length; j++) {
    var el = listsWithHandle[j];
    Sortable.create(el, {
      // handle: ".sortableJS-handle", // only move element when dragging on handle element
      animation: 150,
      onEnd: function (evt) {
        $(el).trigger("end");
    }});
  }
})
