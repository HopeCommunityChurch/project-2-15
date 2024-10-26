"use strict";

(() => {

function styles (time, type) {
  let backgroundColor = (type =="error")? "--red-25" : "--blue-25";
  let borderColor = (type =="error")? "--red-75" : "--blue-75";
  let timerColor = (type =="error")? "--red-100" : "--blue-100";
  return `
    .base {
      background-color: var(${backgroundColor});
      border: 1px solid var(${borderColor});
      border-radius: 5px;
      overflow: hidden;
    }
    .divSlot {
      padding: 2px 5px;
    }
    .timer-holder {
      height: 2px;
      width: 100%;
    }
    .timer {
      background-color: var(${timerColor});
      width: 100%;
      height: 100%;
      animation: shrink ${time/1000}s linear;
    }
    @keyframes shrink {
      from {
        width: 100%;
      }
      to {
        width: 0%;
      }
    }

  `;
}

class Notification extends HTMLElement {

  constructor () {
    self = super();
  }

  connectedCallback() {
    this.shadow = this.attachShadow({mode: "closed"});
    const time = this.getAttribute("time-ms");
    const type = this.getAttribute("type");
    console.log(time);

    const stylesElem = document.createElement("style");
    stylesElem.textContent = styles(time, time);
    this.shadow.appendChild(stylesElem);

    this.base = document.createElement("div");
    this.base.part = "base";
    this.base.className = "base";
    this.shadow.appendChild(this.base);

    this.divSlot = document.createElement("div");
    this.divSlot.className = "divSlot";
    this.base.appendChild(this.divSlot);

    this.timerHolder = document.createElement("div");
    this.timerHolder.className = "timer-holder";
    this.base.appendChild(this.timerHolder);

    this.timer = document.createElement("div");
    this.timer.className = "timer";
    this.timerHolder.appendChild(this.timer);


    setTimeout(() => {
      this.divSlot.innerHTML = this.innerHTML;
    }, 0);

    if (time) {
      setTimeout(() => {
        this.parentNode.removeChild(this);
      }, time);
    }

  }

}

customElements.define("p-notification", Notification);
})();
