$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('validateInput', function (data) {
    var inputId = data.inputId;
    var isValid = data.isValid;
    var message = data.message;

    // Try both CSS selector patterns
    var selector1 = '.shiny-input-container#' + inputId;
    var selector2 = '.shiny-input-container:has(#' + inputId + ')';

    var container = $(selector1);
    if (container.length === 0) {
      container = $(selector2);
    }

    if (container.length > 0) {
      // Remove existing validation message
      container.find('.shiny-output-error').remove();

      // Add validation message if rule failed
      if (!isValid && message && message.trim() !== '') {
        var validationSpan = $('<span>').addClass('shiny-output-error').text(message);
        container.append(validationSpan);
      }
    } else {
      console.warn('Container not found for input: ' + inputId);
    }
  });
});