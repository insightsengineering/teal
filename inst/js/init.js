// This file contains functions that should be executed at the start of each session,
// not included in the original HTML

// this code alows the show R code "copy to clipbaord" button to work
var clipboard = new ClipboardJS('.btn[data-clipboard-target]');

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

waitForElm('div#TEST22').then((elm) => {
  $("a[data-toggle='tab']")[0].click();
});

