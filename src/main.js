// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import { compose, defaultTo, mergeDeepRight } from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'

const items = [
  { id: '1', title: 'One', pid: null, childIds: [] },
  { id: '2', title: 'Two', pid: null, childIds: [] },
  { id: '3', title: 'Three', pid: null, childIds: [] },
  //
]
const elmMainCached = compose(
  mergeDeepRight({ items }),
  defaultTo({}),
  // always(null),
  getCached,
)('elm-main')
const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: elmMainCached,
})

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
})
