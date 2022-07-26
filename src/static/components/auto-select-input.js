customElements.define(
  'select-input',
  class extends HTMLElement {
    #input = null

    constructor() {
      super()

      this.onfocus = (e) => {
        console.log('got focus!')
        e.target.select()
      }

      this.#input = document.createElement('input', {
        onfocus: (e) => e.target.select(),
      })

      // this.#input.setAttribute('style', 'width: 100%; height: 100%; ')
      this.#input.setAttribute('style', 'box-sizing: border-box')

      // this.appendChild(this.#input)
    }

    // render() {
    //   this.outerHTML = `<input type="text"/>`
    // }

    connectedCallback() {
      this.setAttribute(
        'style',
        `padding: 0;
         margin: 0;
         background-color: green;
         box-sizing: border-box;
         display: flex;
         justify-content: stretch;
        `
      )

      // this.#input.setAttribute('class', this.getAttribute('class'))
      // this.#input.setAttribute('style', this.getAttribute('style'))

      // this.setAttribute('class', undefined)
      // this.setAttribute('style', undefined)
      this.appendChild(this.#input)
    }
  }
)

customElements.define(
  'best-text',
  class extends HTMLInputElement {
    constructor() {
      super()

      this.onfocus = (e) => {
        e.preventDefault()
        if (this.getAttribute('no-focus') === null) e.target.select()
      }

      this.outerHTML = `<input type="text"/>`
    }
  },
  { extends: 'input' }
)

// customElements.define(
//   'auto-select-input',
//   class extends HTMLElement {
//     #input = null
//     #value = ''
//     #placeholder = ''

//     setPlaceholder(placeholder) {
//       this.#placeholder = placeholder
//       this.#input?.setAttribute('placeholder', this.#placeholder)
//     }

//     setValue(value) {
//       this.#value = value
//       this.#input?.setAttribute('value', this.#value)
//     }

//     set placeholder(placeholder) {
//       this.setPlaceholder(placeholder)
//     }

//     get placeholder() {
//       return this.#placeholder
//     }

//     set value(value) {
//       this.setValue(value)
//     }

//     get value() {
//       return this.#value
//     }

//     connectedCallback() {
//       this.#input = document.createElement('input')

//       this.#input.onfocus = (e) => {
//         e.target.select()
//       }

//       this.setPlaceholder(this.placeholder)
//       this.setValue(this.value)

//       this.#input?.setAttribute('class', this.class)
//       this.#input?.setAttribute('style', this.style)

//       this.shadowRoot.appendChild(this.#input)
//     }
//   }
// )
