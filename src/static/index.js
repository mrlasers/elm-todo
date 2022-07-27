const { Elm } = require('../elm/Main.elm')

import './components/auto-select-input'
import { nanoid } from 'nanoid'

// class AutoSelectInput extends HtmlElement {
//   connectedCallback() {
//     const input = document.createElement('input')
//     input.onfocus((e) => e.target.select())
//     this.appendChild(input)
//   }
// }

// window.customElements.define('auto-select-input', AutoSelectInput)

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const flags = { seed: getRandomInts(5)[0] }

const app = Elm.Main.init({ node: document.getElementById('app'), flags })

app.ports.sendMessage.subscribe(function (action) {
  console.log('ports.sendMessage:', action)
  switch (action.type) {
    default:
      console.log('Unhandled message from Elm:', action)
    case 'focus': {
      const el = document.getElementById(action.payload)
      if (el) el.select()
      return
    }
    case 'getid': {
      const id = nanoid()

      console.log(`Sending id "${id}" to Elm...`)

      app.ports.messageReceiver.send(
        JSON.stringify({ name: 'getid', payload: id })
      )
    }
  }
})
