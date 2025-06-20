function initNavigationMouseOver() {
  this.classList.add("show");
  this.querySelector(".dropdown-toggle").classList.add("show");
  this.querySelector(".dropdown-toggle").setAttribute("aria-expanded", "true");
  this.querySelector(".dropdown-menu").classList.add("show");
}

function initNavigationMouseOut() {
  this.classList.remove("show");
  this.querySelector(".dropdown-toggle").classList.remove("show");
  this.querySelector(".dropdown-toggle").setAttribute("aria-expanded", "false");
  this.querySelector(".dropdown-menu").classList.remove("show");
}
