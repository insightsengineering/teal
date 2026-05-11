$(document).on('shiny:connected', function () {

  /**
   * Find the .shiny-input-container for a given inputId,
   * then call `callback(container)` once it exists in the DOM.
   * Gives up after `timeoutMs` milliseconds.
   */
  function withContainer(inputId, callback, timeoutMs) {
    timeoutMs = timeoutMs || 5000;

    function findContainer() {
      var container = $('.shiny-input-container#' + inputId);
      if (container.length === 0) {
        container = $('.shiny-input-container:has(#' + inputId + ')');
      }
      return container.length > 0 ? container : null;
    }

    var existing = findContainer();
    if (existing) {
      callback(existing);
      return;
    }

    // Element not yet in DOM — observe until it appears or we time out
    var observer = new MutationObserver(function () {
      var found = findContainer();
      if (found) {
        observer.disconnect();
        clearTimeout(timer);
        callback(found);
      }
    });

    observer.observe(document.body, { childList: true, subtree: true });

    var timer = setTimeout(function () {
      observer.disconnect();
      console.warn('Container not found for input after timeout: ' + inputId);
    }, timeoutMs);
  }

  function applyValidation(container, isValid, message) {
    // Remove existing validation messages from siblings of the container
    container.parent().children('.shiny-input-validation-error').remove()

    // Add UI element for validation message if not valid
    if (!isValid && message && message.trim() !== '') {
      var validationSpan = $('<span>')
        .addClass('shiny-output-error')
        .addClass('shiny-input-validation-error')
        .text(message);
      container.after(validationSpan);
    }

    // Clear validation message on next input change to avoid having stale messages
    container.off('shiny:inputchanged').on('shiny:inputchanged', function () {
      container.find('.shiny-input-validation-error').remove();
    });
  }

  Shiny.addCustomMessageHandler('validateInput', function (data) {
    withContainer(data.inputId, function (container) {
      applyValidation(container, data.isValid, data.message);
    });
  });
});