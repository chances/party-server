import { NestFactory } from '@nestjs/core'
// tslint:disable-next-line:no-implicit-dependencies
import * as express from 'express'
import * as path from 'path'

import { AppModule } from './app.module'
import { EnvironmentService } from './env.service'
import { FatalException } from './fatal.exception'
import { engine, ViewService } from './view.service'

async function bootstrap() {
  const app = await NestFactory.create(AppModule)

  app.use(express.static(path.join(__dirname, '../public')))
  app.set('views', path.join(__dirname, '../views'))
  app.engine('lit', engine(app.get(ViewService)))
  app.set('view engine', 'lit')

  const env = app.get(EnvironmentService)
  await app.listen(env.port)
}
try {
  bootstrap()
} catch (e) {
  if (e instanceof FatalException) {
    process.exit(1)
  }
}
