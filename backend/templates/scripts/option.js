
class PSelect extends HTMLElement {
  static formAssociated = true;

  static _styles = `
    .p-select {
      display: inline-grid;
      grid-template-columns: 1fr 1em;
      position: relative;
      padding: 2px 6px;
      box-sizing: border-box;
      background-color: #e9e9ed;
      border-radius: 5px;
      span {
        justify-self: start;
        align-self: center;
      }
      &:hover {
        background-color: #d0d0d7;
      }

      .p-arrow {
        width: 1em;
        justify-self: end;
        align-self: center;
      }

      .p-dropbox {
        position: absolute;
        background-color: #e9e9ed;
        min-width: 100%;
        display: none;
        box-shadow: 0 2px 4px #0003;
        z-index: 1;
        top: 100%;
        .option{
          padding: 2px 6px;
          display: grid;
          align-content: center;
          white-space: nowrap;
          &:hover {
            background-color: #d0d0d7;
          }
        }
        &.open {
          display: block;
        }
      }
    }
  `;

  constructor () {
    self = super();
  }

  connectedCallback() {

    this.internals = this.attachInternals();

    this.shadow = this.attachShadow({mode: "closed"});
    const styles = document.createElement("style");
    styles.textContent = PSelect._styles;
    this.shadow.appendChild(styles);

    this.select = document.createElement("div");
    this.select.part = "p-select";
    this.select.className = "p-select";
    this.shadow.appendChild(this.select);
    this.current = document.createElement("span");
    this.select.appendChild(this.current);

    this.arrow = document.createElement("img");
    this.arrow.className = "p-arrow";
    this.arrow.src = "{{base}}/static/img/arrow.svg"
    this.select.appendChild(this.arrow);

    this.dropbox = document.createElement("div");
    this.dropbox.part = "p-dropbox";
    this.dropbox.className = "p-dropbox";
    this.select.addEventListener("click", (e) => {
      e.stopPropagation();
      this.dropbox.classList.toggle("open");
    });
    document.addEventListener("click", () => {
      this.dropbox.classList.remove("open");
    });
    this.select.appendChild(this.dropbox);

    setTimeout(() => {
      const options = Array.from(this.querySelectorAll("option"));
      let def = options.find( (option) => option.selected)
      if(def == undefined) {
        if(options.length != 0) {
          def = options[0];
        }
      }

      if(def) {
        this.selected(def);
      }

      options.forEach( (option) => {
        const whatever = document.createElement("div");
        whatever.className = "option";
        whatever.part = "p-option";
        whatever.innerHTML = option.innerText;
        whatever.addEventListener("click", (e) => {
          e.stopPropagation();
          this.selected(option);
        });
        this.dropbox.appendChild(whatever);
      });
    }, 0);

  }

  selected (option) {
    let value = option.value
    if(!value) {
      value = option.innerText;
    }
    this.internals.setFormValue(value);
    this.current.innerHTML = option.innerText;
    this.dropbox.classList.remove("open");
  }

}

customElements.define("p-select", PSelect);
