require("tachyons")
const {Elm} = require('./Main.elm')

console.log(Elm)

const app = Elm.Main.init({
  node: document.querySelector('#main') || document.querySelector('body > *')
})

const intervalId  = setInterval(()=>{
  app.ports.fromJs.send(Math.round(Math.random()*11))
}, 1000000)

console.log(app)

if (module.hot) {
  module.hot.accept((data)=>{
    console.log("data",data)
  })
  module.hot.dispose(data=>{
    data.foo=1
    console.log("data",data)
    clearInterval(intervalId)
  })
}