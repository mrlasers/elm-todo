const { Elm } = require('../elm/Main.elm')

const flags = {
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
