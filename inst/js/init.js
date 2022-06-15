// This file contains functions that should be executed at the start of each session,
// not included in the original HTML

// this code alows the show R code "copy to clipbaord" button to work
var clipboard = new ClipboardJS('.btn[data-clipboard-target]');

// this code alows to click the first tab when then main teal UI is inserted
// it is needed to achieve the full initial shiny input cycle
function wait_for_element(selector) {
    return new Promise(resolve => {
        let init_check = document.querySelector(selector);
        if (init_check) {
            return resolve(init_check);
        }

        const observer = new MutationObserver(() => {
            let obs_check = document.querySelector(selector);
            if (obs_check) {
                resolve(obs_check);
                observer.disconnect();
            }
        });

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
    });
}

wait_for_element('div#teal_main_modules_ui').then(() => {
  $("div#teal_main_modules_ui a[data-toggle='tab']")[0].click();
});

