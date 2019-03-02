// noinspection JSUnresolvedVariable
import { setCache } from './cache-helpers'
import { always, compose, defaultTo, mergeDeepRight } from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'

const items = [
  { id: '1', title: 'One', pid: null },
  { id: '2', title: 'Two', pid: null },
  { id: '3', title: 'Three', pid: null },
  //
]
const elmMainCached = compose(
  mergeDeepRight({ items }),
  defaultTo({}),
  always(null),
  // getCached,
)('elm-main')
const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: elmMainCached,
})

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
})
