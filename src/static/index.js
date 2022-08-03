import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'
import '@webcomponents/webcomponentsjs/custom-elements-es5-adapter.js'
import 'elm-rte-toolkit'
// import 'dialog-polyfill' // installed this polyfill, but won't include it unless that turns out to be necessary

const { Elm } = require('../elm/Main.elm')

const flags = {
  seed: Math.floor(Math.random() * 0xffffffff),
  projects: JSON.parse(localStorage.getItem('projects') ?? '[]'),
}

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags,
})

app.ports.messageFromElm.subscribe((msg) => {
  const { type, payload } = msg

  switch (type) {
    default:
      console.log('Got a message from Elm:', msg)
      return
    case 'focus-element':
      document.getElementById(payload)?.select()
      console.log('focus-element', payload)
      return
    case 'save-todos':
      localStorage.setItem('todos', JSON.stringify(payload))
      return
    case 'save-projects':
      console.log('saving projects', payload)
      localStorage.setItem('projects', JSON.stringify(payload))
      return
    case 'show-modal': {
      const modal = document.getElementById(payload)
      modal?.showModal()
      return
    }
    case 'hide-modal': {
      const modal = document.getElementById(payload)
      modal?.close()
      return
    }
  }
})
