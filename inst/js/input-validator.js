$(document).on('shiny:connected', function () {
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
        console.log("found!", found);
        observer.disconnect();
        clearTimeout(timer);
        callback(found);
      }
      console.log("not found yet");
    });

    observer.observe(document.body, { childList: true, subtree: true });

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
