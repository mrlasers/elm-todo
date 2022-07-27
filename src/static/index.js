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

app.ports.messageReceiver.send(
  JSON.stringify({
    type: 'hello',
    payload: 'howdy doody',
  })
)

// console.log(app.ports)

// app.ports.sendMessage.subscribe(function (message) {
//   console.log('Received message from `app.ports.sendMessage`: ', message)
// })

app.ports.messageFromElm.subscribe(function (message) {
  console.log('Message type from Elm:', message.type)
  switch (message.type) {
    default:
      return console.log('Received unknown message type from Elm:', message)
    case 'focus-element':
      return document.getElementById(message.payload)?.select()
  }
  // const message = JSON.parse(rawjson)

  // Promise.resolve()
  //   .then((_) => JSON.parse(json))
  //   .then((result) => console.log('Received JSON message from Elm', result))
  //   .catch((err) =>
  //     console.error(
  //       "Received something that isn't JSON from Elm:\n",
  //       'Message:',
  //       json,
  //       '\n',
  //       'ERROR:',
  //       err.message
  //     )
  //   )

  // // console.log('ports.sendMessage:', message)
  // switch (message.type) {
  //   default:
  //     return console.log('Unhandled message from Elm:', message)

  //   case 'focus-element': {
  //     const el = document.getElementById(message.payload)

  //     console.log('el', el)

  //     console.log(`Received message type "${message.type}" from Elm...`, el)
  //     if (el) {
  //       el.select()

  //       el.value = 'fuckc it all'
  //     }
  //     return
  //   }
  //   case 'getid': {
  //     const id = nanoid()

  //     console.log(`Sending id "${id}" to Elm...`)

  //     app.ports.messageReceiver.send(
  //       JSON.stringify({ name: 'getid', payload: id })
  //     )
  //   }
  // }
})
