var clipboard = new ClipboardJS('.btn[data-clipboard-target]');
clipboard.on('success', function(e) {
    console.log(e);
});
clipboard.on('error', function(e) {
    console.log(e);
});

var myAccordion = new Accordion('.accordion-container');
