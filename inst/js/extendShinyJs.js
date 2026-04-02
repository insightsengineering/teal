// Custom message handler to get document title and send it to the server
Shiny.addCustomMessageHandler('teal-get-document-title', function(message) {
  const title = document.title;
  const inputId = message.inputId;
  console.log('Teal: Sending document title:', title, 'to input:', inputId);
  Shiny.setInputValue(inputId, title, {priority: 'event'});
});

