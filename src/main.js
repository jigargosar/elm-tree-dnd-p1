// noinspection JSUnresolvedVariable
import { getCached, setCache } from './cache-helpers'
import { compose, defaultTo, isEmpty, mergeDeepRight, omit } from 'ramda'
import './main.scss'
import { Elm } from './Main.elm'
import PouchDb from 'pouchdb-browser'
import validate from 'aproba'

// const items = times(createNewItem)(3)
//
// function createNewItem() {
//   return {
//     id: 'i_' + nanoid(),
//     rev: null,
//     title: faker.lorem.words(),
//     pid: null,
//     childIds: [],
//     rootIdx: -1,
//   }
// }

const elmMainCached = compose(
  mergeDeepRight({ items: [] }),
  defaultTo({}),
  // always(null),
  getCached,
)('elm-main')

const app = Elm.Main.init({
  node:
    document.querySelector('#main') || document.querySelector('body > *'),
  flags: elmMainCached,
})

const db = new PouchDb('items')

db.allDocs({ include_docs: true }).then(({ rows }) =>
  app.ports.pouchItemsLoaded.send(
    rows.map(r => {
      const doc = r.doc
      const id = doc._id
      const rev = doc._rev
      return { id, rev, ...doc }
    }),
  ),
)

db.changes({ include_docs: true, live: true, since: 'now' })
  .on('change', change => {
    console.log(change)
  })
  .on('error', error => console.error('item changes error', error))

app.ports.toJsCache.subscribe(model => {
  setCache('elm-main', model)
})

function bulkItemDocs(items) {
  validate('A', arguments)

  console.log('bulkItemDocs: items', items)
  if (!isEmpty(items)) {
    const docs = items.map(item => ({
      _id: item.id,
      _rev: item.rev,
      ...omit(['id', 'rev'])(item),
    }))
    console.log('bulkItemDocs: docs', docs)
    db.bulkDocs(docs)
      .then(res => console.log('ports.bulkItemDocs res', res))
      .catch(console.error)
  }
}

app.ports.bulkItemDocs.subscribe(bulkItemDocs)

// const debouncedBulkItemDocs = debounce(bulkItemDocs, 1000, {
//   leading: false,
//   trailing: true,
// })
//
// app.ports.debouncedBulkItemDocs.subscribe(debouncedBulkItemDocs)

if (module.hot) {
  module.hot.accept(() => window.location.reload(true))
}
