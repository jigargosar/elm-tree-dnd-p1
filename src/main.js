// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import { compose, defaultTo, mergeDeepRight } from 'ramda'

// noinspection JSUnresolvedFunction
require('./main.scss')
// noinspection JSUnresolvedFunction
const { Elm } = require('./Main.elm')

const items = [
  { id: '1', title: 'One' },
  { id: '2', title: 'Two' },
  { id: '3', title: 'Three' },
  //
]
const elmMainCached = compose(
  mergeDeepRight({ items }),
  defaultTo({}),
)(getCached('elm-main'))
const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: elmMainCached,
})

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
})
