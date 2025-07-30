$(document).on("shiny:busy", function () {
  $(".teal-busy-disable").prop("disabled", true);
});
$(document).on("shiny:idle", function () {
  $(".teal-busy-disable").prop("disabled", false);
});
