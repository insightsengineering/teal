// This file contains functions that should be executed at the start of each session,
// not included in the original HTML

// this code alows the show R code "copy to clipbaord" button to work
var clipboard = new ClipboardJS('.btn[data-clipboard-target]');

// https://stackoverflow.com/a/61511955
function waitForElm(selector) {
    return new Promise(resolve => {
        if (document.querySelector(selector)) {
            return resolve(document.querySelector(selector));
        }

        const observer = new MutationObserver(mutations => {
            if (document.querySelector(selector)) {
                resolve(document.querySelector(selector));
                observer.disconnect();
            }
        });

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
}

waitForElm('div#teal_main_modules_ui').then((elm) => {
  $("div#teal_main_modules_ui a[data-toggle='tab']")[0].click();
});

