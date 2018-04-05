import { Module } from '@nestjs/common'

import { AppController } from './app.controller'
import { EnvironmentService } from './env.service'
import { ViewService } from './view.service'

@Module({
  imports: [],
  controllers: [AppController],
  components: [EnvironmentService, ViewService],
})
export class AppModule {}
