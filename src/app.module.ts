import { Module } from '@nestjs/common'

import { AppController } from './app.controller'
import { EnvironmentService } from './env.service'

@Module({
  imports: [],
  controllers: [AppController],
  components: [EnvironmentService],
})
export class AppModule {}
