// When invoked it adds the setClass and removes the removeClass from the element.
function setAndRemoveClass(element, setClass, removeClass) {
  if (typeof element === "string") {
    element = document.querySelector(element);
  }
  element.classList.add(setClass);
  element.classList.remove(removeClass);
}

// When invoked it toggles the class of the element.
function toggleClass(element, class1, class2) {
  if (typeof element === "string") {
    element = document.querySelector(element);
  }
  if (element.classList.contains(class1)) {
    setAndRemoveClass(element, class2, class1);
  } else {
    setAndRemoveClass(element, class1, class2);
  }
}

// When invoked it shows the targetSelector element.
function showPanelItem(targeSelector, duration = 400, easing = "slideInTop") {
  $(`#${targeSelector}`).show(duration, easing);
  $(`#${targeSelector}`).trigger("shown");
}

// When invoked it hides the targetSelector element.
function hidePanelItem(targeSelector, duration = 400, easing = "slideOutLeft") {
  $(`#${targeSelector}`).hide(duration, easing);
}

// When invoked it hides/shows targetSelectors elements
// and changes class of element from class1 <-> class2
function togglePanelItems(
  element,
  targetSelectors,
  class1,
  class2,
  duration = 400,
  easing = "swing"
) {
  if (!Array.isArray(targetSelectors)) {
    targetSelectors = [targetSelectors];
  }

  targetSelectors.forEach((targetSelector) => {
    if ($(`#${targetSelector}`).is(":visible")) {
      hidePanelItem(targetSelector, duration, easing);
    } else {
      showPanelItem(targetSelector, duration, easing);
    }
  });

  toggleClass(element, class1, class2);
}
