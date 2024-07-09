// used to collapse and expand the filter panel in teal apps
var filter_open = true;
const hideSidebar = () => {
  $(".teal_secondary_col").fadeOut(1);
  $(".teal_primary_col").removeClass("col-sm-9").addClass("col-sm-12");
};
const showSidebar = () => {
  $(".teal_primary_col").removeClass("col-sm-12").addClass("col-sm-9");
  $(".teal_secondary_col").fadeIn(650);
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

// Function to hide filter panel and disable the burger button
const handleNoActiveDatasets = () => {
  $(".filter_hamburger").addClass("hidden");
  $(".filter_manager_button").addClass("hidden");
  hideSidebar();
};
// Function to show filter panel and enable the burger button
const handleActiveDatasetsPresent = () => {
  $(".filter_hamburger").removeClass("hidden");
  $(".filter_manager_button").removeClass("hidden");
  if (filter_open) showSidebar();
}
