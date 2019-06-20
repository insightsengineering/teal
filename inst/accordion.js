/**
 * Accordion v1.0.0
 * Fancy accordion with Javascript
 * https://github.com/denizhacisalihoglu/accordion-init
 *
 * Copyright 2018 Deniz Hacısalihoğlu
 * Published under MIT License
 */

function Accordion (element, newSettings) {
  var accordion = this
  var settings, classes
  if (!newSettings) {
    settings = {
      hideAll: false,
      showAll: false,
      showFirst: false,
      panelId: null }
    classes = {
      container: '.accordion-container',
      panel: '.panel > .heading'
    }
  } else {
    settings = newSettings
    classes = {
      container: element,
      panel: '.panel > .heading'
    }
  };

  accordion.settings = settings
  accordion.classes = classes

  var panels = document.querySelectorAll(accordion.classes.panel)

  panels.forEach(function (panel) {
    panel.addEventListener('click', function () {
      accordion.toggleParentPanel(panel)
    })
  })

  this.togglePanel = function (panel) {
    panel.classList.toggle('active')
    var heading = panel.children[1]
    if (heading.style.maxHeight) {
      heading.style.maxHeight = null
    } else {
      heading.style.maxHeight = heading.scrollHeight + 'px'
    }
  }

  this.togglePanelById = function (id) {
    var panel = document.querySelectorAll('.panel')[id]
    accordion.togglePanel(panel)
  }

  this.toggleParentPanel = function (heading) {
    var panel = heading.parentElement
    accordion.togglePanel(panel)
  }

  this.showAll = function () {
    panels.forEach(function (panel) {
      panel.classList.add('active')
      var heading = panel.children[1]
      heading.style.maxHeight = heading.scrollHeight + 'px'
    })
  }

  this.hideAll = function () {
    panels.forEach(function (panel) {
      panel.classList.remove('active')
      var heading = panel.children[1]
      heading.style.maxHeight = null
    })
  }

  if (accordion.settings.showAll) accordion.showAll()
  if (accordion.settings.hideAll) accordion.hideAll()
  if (accordion.settings.showFirst) accordion.togglePanelById(parseInt(accordion.settings.panelId))
};
