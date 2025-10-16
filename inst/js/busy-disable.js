// Primarily added to make sure the user does not open multiple modals when shiny is busy.
// https://github.com/rstudio/shiny/issues/4261
$(document).on("shiny:busy", function () {
  $(".teal-busy-disable").prop("disabled", true);
});
$(document).on("shiny:idle", function () {
  $(".teal-busy-disable").prop("disabled", false);
});
