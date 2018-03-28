import { expect, test, that } from '../test/expect'

test('get returns undefined for non-existent env variable', t => {
  expect(that('foo').equals('foo'))
  t.end()
})
