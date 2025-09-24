function toggleBadgeDropdown(summaryId, containerId) {
  var container = document.getElementById(containerId);
  var summary = document.getElementById(summaryId);

  if(container.style.visibility === 'hidden' || container.style.visibility === '') {
    container.style.visibility = 'visible';
    container.style.opacity = '1';
    container.style.pointerEvents = 'auto';
    $(container).trigger('shown');
    Shiny.bindAll(container);

    // Add click outside handler
    setTimeout(function() {
      function handleClickOutside(event) {
        if (!container.contains(event.target) && !summary.contains(event.target)) {
          container.style.visibility = 'hidden';
          container.style.opacity = '0';
          container.style.pointerEvents = 'none';
          $(container).trigger('hidden');
          document.removeEventListener('click', handleClickOutside);
        }
      }
      document.addEventListener('click', handleClickOutside);
    }, 10);
  } else {
    container.style.visibility = 'hidden';
    container.style.opacity = '0';
    container.style.pointerEvents = 'none';
    $(container).trigger('hidden');
  }
}