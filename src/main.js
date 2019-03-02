// noinspection JSUnresolvedVariable
const m = module

require('./main.scss')
const { Elm } = require('./Main.elm')

console.log(Elm)

const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: {
    items: [
      { id: '1', title: 'One' },
      { id: '2', title: 'Two' },
      { id: '3', title: 'Three' },
      //
    ],
  },
})

const intervalId = setInterval(() => {
  app.ports.fromJs.send(Math.round(Math.random() * 11))
}, 1000000)

console.log(app)

if (m.hot) {
  m.hot.accept(data => {
    console.log('data', data)
  })
  m.hot.dispose(data => {
    data.foo = 1
    console.log('data', data)
    clearInterval(intervalId)
  })
}
