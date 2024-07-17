// This function handles validation errors displayed in the teal app
//  - If error is printed in a div.teal_validate (see validate_reactive_teal_data) then the parent element
//  will be surrounded by a red border if the div.teal_validate has content (when error occurs) 


// Function to update the parent div's border based on the element's content
function updateParentBorder(element) {
  const parentDiv = element.closest('.teal_validated');
  if (parentDiv) {
    // Change the border of the parent div to red if the element has content
    parentDiv.style.border = element.textContent.trim() !== '' || element.children.length > 0 ? '1px solid red' : '';
  }
}

// Observe changes in elements with the class .teal_validate
function observeTealValidateElements() {
  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      if (mutation.type === 'childList' || mutation.type === 'characterData') {
        updateParentBorder(mutation.target);
      }
    });
  });

  const config = {
    childList: true,
    characterData: true,
    subtree: true
  };

  // Select all elements with the class .teal_validate and observe each
  const elements = document.querySelectorAll('div.teal_validate');
  elements.forEach(element => {
    updateParentBorder(element); // Initial check for content
    observer.observe(element, config);
  });
}



// Start observing
document.addEventListener('DOMContentLoaded', function () {
  observeTealValidateElements();
});