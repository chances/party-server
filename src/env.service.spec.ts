import { Test } from '@nestjs/testing'
import { expect, test, that } from '../test/expect'
import { EnvironmentService } from './env.service'
import { FatalException } from './fatal.exception'

async function getService(): Promise<EnvironmentService> {
  const mod = await Test.createTestingModule({
    components: [EnvironmentService],
  }).compile()

  return mod.get(EnvironmentService)
}

test('get gets env variable', t => {
  getService().then(env => {
    process.env.foo = 'bar'
    expect(that(env.get('foo')).equals('bar'))

    process.env.foo = undefined
    delete process.env.foo
    t.end()
  })
})

test('get returns undefined for non-existent env variable', t => {
  getService().then(env => {
    expect(that(env.get('foo')).is.undefined)
    t.end()
  })
})

function withEnv(key: string, value: any, cb: (key: string, value: string) => void) {
  process.env[key] = `${value}`
  cb(key, value)
  process.env[key] = undefined
  delete process.env[key]
}

test('getOrFatal gets env variable', t => {
  getService().then(env => {
    withEnv('foo', 'bar', (key, value) => {
      expect(that(env.getOrFatal(key)).equals(value))
    })
    t.end()
  })
})

function expectFatalException(key: string, cb: () => void) {
  expect(that(cb).throws(FatalException, `Missing environment variable: ${key}`))
}

test('getOrFatal throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'foo'
    expectFatalException(key, () => env.getOrFatal(key))
    t.end()
  })
})

test('port defaults to 3000', t => {
  getService().then(env => {
    delete process.env.PORT
    expect(that(env.port).equals(3000))
    t.end()
  })
})

test('port may be 3005', t => {
  getService().then(env => {
    withEnv('PORT', 3005, (key, expectedValue) => {
      const value = parseInt(env.getOrFatal(key), 10)
      expect(that(value).is.not.NaN)
      expect(that(value).equals(expectedValue))
    })
    t.end()
  })
})

test('corsOrigins defaults to https://chancesnow.me', t => {
  getService().then(env => {
    const origins = env.corsOrigins
    expect(that(origins).has.length(1).and.has.members(['https://chancesnow.me']))
    t.end()
  })
})

test('databaseUrl throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'DATABASE_URL'
    expectFatalException(key, () => env.databaseUrl)
    t.end()
  })
})

test('redisUrl throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'REDIS_URL'
    expectFatalException(key, () => env.redisUrl)
    t.end()
  })
})

test('spotifyAppKey throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'SPOTIFY_APP_KEY'
    expectFatalException(key, () => env.spotifyAppKey)
    t.end()
  })
})

test('spotifyAppSecret throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'SPOTIFY_APP_SECRET'
    expectFatalException(key, () => env.spotifyAppSecret)
    t.end()
  })
})

test('spotifyCallback throws for non-existent env variable', t => {
  getService().then(env => {
    const key = 'SPOTIFY_CALLBACK'
    expectFatalException(key, () => env.spotifyCallback)
    t.end()
  })
})
