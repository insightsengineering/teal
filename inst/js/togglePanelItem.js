// when invoked it hides/show targetId element and change class of element from class1 <-> class2
function togglePanelItem(element, targetId, class1, class2) {
  var target = document.getElementById(targetId);
  if (target.style.display === "none") {
    target.style.display = "block";
    element.classList.remove(class1);
    element.classList.add(class2);
  } else {
    target.style.display = "none";
    element.classList.remove(class2);
    element.classList.add(class1);
  }
}
