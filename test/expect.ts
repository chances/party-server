import * as chai from 'chai'
import tape = require('tape')

let topic: tape.Test

export function test(name: string, cb: tape.TestCase) {
  tape(name, t => {
    try {
      topic = t
      cb(t)
    } catch (e) {
      topic.fail(e.message)
    }
  })
}

export const skip = tape.skip

export function expect(assertion: Chai.Assertion) {
  if (topic == null) {
    throw new Error('Test has no topic subject')
  }

  topic.ok(assertion)
}

export const that = chai.expect
