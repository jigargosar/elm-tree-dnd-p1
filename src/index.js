require("tachyons")
const {Elm} = require('./Main.elm')

console.log(Elm)

Elm.Main.init({
  node: document.querySelector('#main')
})