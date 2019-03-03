// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import { compose, defaultTo, isEmpty, mergeDeepRight, times } from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'
import PouchDb from 'pouchdb-browser'
import faker from 'faker'
import nanoid from 'nanoid'
import validate from 'aproba'

const items = times(createNewItem)(3)

function createNewItem() {
  return {
    id: 'i_' + nanoid(),
    title: faker.lorem.words(),
    pid: null,
    childIds: [],
  }
}

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

const db = new PouchDb('items')

// db.allDocs({include_docs:true})

app.ports.bulkItemDocs.subscribe(function(items) {
  validate('A', arguments)

  console.log('items', items)
  if (!isEmpty(items)) {
    const docs = items.map(item => ({
      _id: item.id,
      _rev: item.rev,
      ...item,
    }))
    db.bulkDocs(docs)
      .then(res => console.log('ports.bulkItemDocs res', res))
      .catch(console.error)
  }
})

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
