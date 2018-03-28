import { Component, Logger } from '@nestjs/common'
import * as dotenv from 'dotenv'

@Component()
export class EnvironmentService {
  private readonly logger = new Logger(this.constructor.name)

  constructor() {
    dotenv.config()
  }

  get port() {
    const port = this.get('PORT', '3000')
    if (port === undefined) { return 3000 }
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

  private get(key: string, defaultValue?: string) {
    return process.env[key] || defaultValue
  }

  private getOrFatal(key: string) {
    const value = this.get(key)
    if (value === undefined) {
      this.logger.error(`Missing environment variable: ${key}`, '')
      process.exit(1)
      return ''
    }
    return value
  }
}
