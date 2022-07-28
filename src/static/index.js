const { Elm } = require('../elm/Main.elm')

import { nanoid } from 'nanoid'

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const flags = {
  seed: getRandomInts(5)[0],
  todos: JSON.parse(localStorage.getItem('todos')) ?? [],
}

const app = Elm.Main.init({ node: document.getElementById('app'), flags })

app.ports.messageReceiver.send(
  JSON.stringify({
    type: 'hello',
    payload: 'howdy doody',
  })
)

app.ports.messageFromElm.subscribe(function ({ type, payload }) {
  switch (type) {
    default:
      return console.log(
        type
          ? `Received unknown message of ${type} from Elm:`
          : `Receeived unknown message from Elm:`,
        message
      )
    case 'focus-element':
      return document.getElementById(payload)?.select()
    case 'save-todos':
      localStorage.setItem('todos', JSON.stringify(payload))
  }
})
