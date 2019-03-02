// noinspection JSUnresolvedVariable
const m = module

// noinspection JSUnresolvedFunction
require('./main.scss')
// noinspection JSUnresolvedFunction
const { Elm } = require('./Main.elm')

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

/*const intervalId = */ setInterval(() => {
  app.ports.fromJs.send(Math.round(Math.random() * 11))
}, 1000000)

// if (m.hot) {
//   m.hot.accept(data => {
//     // console.log('data', data)
//   })
//   m.hot.dispose(data => {
//     data.foo = 1
//     // console.log('data', data)
//     clearInterval(intervalId)
//   })
// }
