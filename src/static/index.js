const { Elm } = require('../elm/Main.elm')

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n)
  crypto.getRandomValues(randInts)
  return Array.from(randInts)
}

const flags = { seed: getRandomInts(5)[0] }

Elm.Main.init({ node: document.getElementById('app'), flags })
