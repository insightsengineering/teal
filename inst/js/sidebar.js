// used to collapse and expand the filter panel in teal apps
/*
resize is placed at end of functions
b/c in embedded apps it will throw errors that cause the function to exit early
*/
var filter_open = {};
const hideSidebar = (tabpanel_wrapper) => {
  $(`#${tabpanel_wrapper} .teal_secondary_col`).fadeOut(1);
  $(`#${tabpanel_wrapper} .teal_primary_col`)
    .removeClass("col-sm-9")
    .addClass("col-sm-12");
};
const showSidebar = (tabpanel_wrapper) => {
  $(`#${tabpanel_wrapper} .teal_primary_col`)
    .removeClass("col-sm-12")
    .addClass("col-sm-9");
  $(`#${tabpanel_wrapper} .teal_secondary_col`).fadeIn(650);
  $(`#${tabpanel_wrapper} .teal_secondary_col`).trigger("shown");
};
const toggleFilterPanel = (tabpanel_wrapper) => {
  if (filter_open[tabpanel_wrapper] === undefined) {
    filter_open[tabpanel_wrapper] = true;
  }
  if (
    filter_open[tabpanel_wrapper] &&
    !$(`#${tabpanel_wrapper} .teal_secondary_col`).is(":visible")
  ) {
    showSidebar(tabpanel_wrapper);
    return;
  }
  filter_open[tabpanel_wrapper] = !filter_open[tabpanel_wrapper];
  if (filter_open[tabpanel_wrapper]) showSidebar(tabpanel_wrapper);
  else hideSidebar(tabpanel_wrapper);
};
