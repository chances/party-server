import { Component, Logger } from '@nestjs/common'
import * as dotenv from 'dotenv'
import { FatalException } from './fatal.exception'

@Component()
export class EnvironmentService {
  private readonly logger = new Logger(this.constructor.name)

  constructor() {
    dotenv.config()
  }

  get port() {
    const port = this.get('PORT', '3000') as string
    return parseInt(port, 10)
  }

  get corsOrigins() {
    const origins = this.get('CORS_ORIGINS', 'https://chancesnow.me') as string
    return origins.split(',')
  }

  get databaseUrl() {
    return this.getOrFatal('DATABASE_URL')
  }

  get redisUrl() {
    return this.getOrFatal('REDIS_URL')
  }

  get spotifyAppKey() {
    return this.getOrFatal('SPOTIFY_APP_KEY')
  }

  get spotifyAppSecret() {
    return this.getOrFatal('SPOTIFY_APP_SECRET')
  }

  get spotifyCallback() {
    return this.getOrFatal('SPOTIFY_CALLBACK')
  }

  get(key: string, defaultValue?: string) {
    return process.env[key] || defaultValue
  }

  getOrFatal(key: string) {
    const value = this.get(key)
    if (value === undefined) {
      const message = `Missing environment variable: ${key}`
      this.logger.error(message, '')
      throw new FatalException(message)
    }
    return value
  }
}
