Shiny.addCustomMessageHandler('disable-tooltip', function(id) {
  const el = document.getElementById(id);
  if (el) {
    const tip = bootstrap.Tooltip.getInstance(el);
    if (tip) {
      tip.disable();
    }
  }
});

Shiny.addCustomMessageHandler('enable-tooltip', function(id) {
  const el = document.getElementById(id);
  if (el) {
    const tip = bootstrap.Tooltip.getInstance(el);
    if (tip) {
      tip.enable();
    }
  }
});