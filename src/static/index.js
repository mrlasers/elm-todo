import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
import '@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js'
import 'elm-rte-toolkit'

const { Elm } = require('../elm/Main.elm')

const flags = {
  seed: Math.floor(Math.random() * 0xffffffff),
  todos: JSON.parse(localStorage.getItem('todos') ?? '[]'),
}

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags,
})

app.ports.messageFromElm.subscribe((msg) => {
  const { type, payload } = msg

  switch (type) {
    default:
      return console.log('Got a message from Elm:', msg)
    case 'focus-element':
      return document.getElementById(payload)?.select()
    case 'save-todos':
      return localStorage.setItem('todos', JSON.stringify(payload))
  }
})
