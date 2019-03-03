// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import {
  always,
  compose,
  defaultTo,
  isEmpty,
  mergeDeepRight,
  times,
} from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'
import PouchDb from 'pouchdb-browser'
import faker from 'faker'
import nanoid from 'nanoid'
import validate from 'aproba'
import debounce from 'lodash.debounce'

const items = times(createNewItem)(3)

function createNewItem() {
  return {
    id: 'i_' + nanoid(),
    title: faker.lorem.words(),
    pid: null,
    childIds: [],
    rootIdx: 0,
  }
}

const elmMainCached = compose(
  mergeDeepRight({ items }),
  defaultTo({}),
  always(null),
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

function bulkItemDocs(items) {
  validate('A', arguments)

  console.log('bulkItemDocs: items', items)
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
}

// app.ports.bulkItemDocs.subscribe(bulkItemDocs)

const debouncedBulkItemDocs = debounce(bulkItemDocs, 1000, {
  leading: false,
  trailing: true,
})

app.ports.debouncedBulkItemDocs.subscribe(debouncedBulkItemDocs)

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
