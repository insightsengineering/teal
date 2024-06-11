// used to collapse and expand the filter panel in teal apps
/*
resize is placed at end of functions
b/c in embedded apps it will throw errors that cause the function to exit early
*/
var filter_open = true;
const hideSidebar = () => {
  $(".teal_secondary_col").css("display", "none");
  $(".teal_primary_col").attr("class", "teal_primary_col col-sm-12");
  $(".teal_primary_col").resize();
};
const showSidebar = () => {
  $(".teal_primary_col").attr("class", "teal_primary_col col-sm-9");
  setTimeout(
    () => {
      $(".teal_secondary_col").css("display", "block");
    },
    600);
  $(".teal_primary_col").resize();
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
