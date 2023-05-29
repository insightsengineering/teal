// used to collapse and expand the filter panel in teal apps
var filter_open = true;
const hideSidebar = () => {
  $(".teal_secondary_col").fadeOut(1);
  $(".teal_primary_col").attr("class", "col-sm-12").resize();
};
const showSidebar = () => {
  $(".teal_primary_col").attr("class", "col-sm-9").resize();
  $(".teal_secondary_col").delay(600).fadeIn(50);
};
const toggleFilterPanel = () => {
  if (
    filter_open &&
    getComputedStyle(document.getElementById("teal_secondary_col")).display ===
      "none"
  ) {
    showSidebar();
    return;
  }
  filter_open = !filter_open;
  if (filter_open) showSidebar();
  else hideSidebar();
};

// Function to hide filter panel and disable the burger button
const handleNoActiveDatasets = () => {
  $(".filter_hamburger").addClass("disabled");
  hideSidebar();
};
// Function to show filter panel and enable the burger button
const handleActiveDatasetsPresent = () => {
  $(".filter_hamburger").removeClass("disabled");
  if (filter_open) showSidebar();
}
