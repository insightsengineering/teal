// This file contains functions that should be executed at the start of each session,
// not included in the original HTML

console.log('extendShinyJs.js loading...');
shinyjs.autoFocusModal = function(id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => document.getElementById(id).focus(),
    { once: true }
  );
}

shinyjs.enterToSubmit = function(id, submit_id) {
  document.getElementById('shiny-modal').addEventListener(
    'shown.bs.modal',
    () => document.getElementById(id).addEventListener('keyup', (e) => {
      if (e.key === 'Enter') {
        e.preventDefault(); // prevent form submission
        document.getElementById(submit_id).click();
      }
    })
  );
}

shinyjs.updateAttribute = function(params) {

  var defaultParams = {
    id : null,
    attr : null,
    value : null
  };
  params = shinyjs.getParams(params, defaultParams);

  document.getElementById(params.id).setAttribute(params.attr, params.value)
}
console.log('extendShinyJs.js loaded', shinyjs);
