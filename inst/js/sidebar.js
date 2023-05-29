// used to collapse and expand the filter panel in teal apps
var filter_open = true;
const hideSidebar = () => {
  $(".teal_secondary_col").fadeOut(1);
  $(".teal_primary_col").attr("class", "teal_primary_col col-sm-12").resize();
};
const showSidebar = () => {
  debugger;
  $(".teal_primary_col").attr("class", "teal_primary_col col-sm-9").resize();
  $(".teal_secondary_col").delay(600).fadeIn(50);
};
const toggleFilterPanel = () => {
  if (filter_open && !$(".teal_secondary_col").is(':visible')) {
    showSidebar();
    return;
  }
  filter_open = !filter_open;
  if (filter_open) showSidebar();
  else hideSidebar();
};