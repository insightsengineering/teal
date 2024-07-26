// used to collapse and expand the filter panel in teal apps
/*
resize is placed at end of functions
b/c in embedded apps it will throw errors that cause the function to exit early
*/
var filter_open = {};
const hideSidebar = (teal_body) => {
  $(`#${teal_body} .teal_secondary_col`).fadeOut(1);
  $(`#${teal_body} .teal_primary_col`)
    .removeClass("col-sm-9")
    .addClass("col-sm-12");
};
const showSidebar = (teal_body) => {
  $(`#${teal_body} .teal_primary_col`)
    .removeClass("col-sm-12")
    .addClass("col-sm-9");
  $(`#${teal_body} .teal_secondary_col`).fadeIn(650);
};
const toggleFilterPanel = (teal_body) => {
  console.log(`#${teal_body} .teal_secondary_col`);
  console.log(filter_open[teal_body] === undefined);
  if (filter_open[teal_body] === undefined) {
    filter_open[teal_body] = true;
  }
  if (
    filter_open[teal_body] &&
    !$(`#${teal_body} .teal_secondary_col`).is(":visible")
  ) {
    showSidebar(teal_body);
    return;
  }
  filter_open[teal_body] = !filter_open[teal_body];
  if (filter_open[teal_body]) showSidebar(teal_body);
  else hideSidebar(teal_body);
};
