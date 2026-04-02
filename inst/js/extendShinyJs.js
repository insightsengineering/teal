// This file contains functions that should be executed at the start of each session,
// not included in the original HTML

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

// Custom message handler to get document title and send it to the server
Shiny.addCustomMessageHandler('teal-get-document-title', function(message) {
  const title = document.title;
  const inputId = message.inputId;
  console.log('Teal: Sending document title:', title, 'to input:', inputId);
  Shiny.setInputValue(inputId, title, {priority: 'event'});
});

