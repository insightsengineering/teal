$(document).on('shiny:connected', function () {
  function withContainer(inputId, callback, timeoutMs) {
    timeoutMs = timeoutMs || 5000;

    function findContainer() {
      // First try direct match
      var container = $('.shiny-input-container#' + inputId);
      if (container.length === 0) {
        // Then try to find container that has this input
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

    // More efficient approach: find a relevant parent container to observe instead of the entire body
    function findTargetContainer() {
      // Try to find a container with an ID that partially matches inputId
      var idComponents = inputId.split('-');

      // Look for potential parent containers by checking IDs that partially match
      for (var i = idComponents.length; i >= 1; i--) {
        var partialId = idComponents.slice(0, i).join('-');
        var potentialContainer = $('#' + partialId);
        if (potentialContainer.length > 0) {
          // Return the closest shiny container or the element itself
          var shinyContainer = potentialContainer.closest('.shiny-bound-input, .shiny-input-container, .form-group');
          return shinyContainer.length > 0 ? shinyContainer[0] : potentialContainer[0];
        }
      }

      // Last resort: observe the body (but this should rarely happen)
      return document.body;
    }

    // Observe only the relevant container instead of the entire body
    var targetContainer = findTargetContainer();
    observer.observe(targetContainer, { childList: true, subtree: true });

    var timer = setTimeout(function () {
      observer.disconnect();
      console.warn('Container not found for input after timeout: ' + inputId);
    }, timeoutMs);
  }

  function applyValidation(container, inputId, isValid, message, fingerprint) {
    // Remove existing validation messages from siblings of the container
    const data_ref = `${inputId}-${fingerprint}`;
    const child_selector = `.shiny-input-validation-error[data-ref="${data_ref}"]`;
    container.parent().children(child_selector).remove();

    // Add UI element for validation message if not valid
    if (!isValid && message && message.trim() !== '') {
      const validationSpan = $('<span>')
        .attr('data-ref', data_ref)
        .addClass('shiny-output-error')
        .addClass('shiny-input-validation-error')
        .text(message);
      container.after(validationSpan);
    }

    // Clear validation message on next input change to avoid having stale messages
    const found = container.find(`#${inputId}`);
    const inputEl = found.length ? found : container;
    let previousValue = inputEl.val();
    // Only remove if the value really changed, not just a re-render
    inputEl.off('shiny:inputchanged').one('shiny:inputchanged', function (event) {
      if (event.name !== inputEl.attr('id')) {
        return;
      }
      // Only clear if the value actually changed
      if (JSON.stringify(event.value) === JSON.stringify(previousValue)) {
        return;
      }
      previousValue = event.value;
      container.parent().children(child_selector).remove() // Remove validation message
    });
  }

  Shiny.addCustomMessageHandler('validateInput', function (data) {
    withContainer(data.inputId, function (container) {
      applyValidation(container, data.inputId, data.isValid, data.message, data.fingerprint);
    });
  });
});
