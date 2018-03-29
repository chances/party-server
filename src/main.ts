import { NestFactory } from '@nestjs/core'

import { AppModule } from './app.module'
import { EnvironmentService } from './env.service'

async function bootstrap() {
  const app = await NestFactory.create(AppModule)
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
