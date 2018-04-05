import { Controller, Get, Render } from '@nestjs/common'
import { IndexView } from './models/views'

@Controller()
export class AppController {
  @Get()
  @Render('index')
  root(): IndexView {
    return {
      user: null,
      currentParty: null,
      currentPlaylist: null,
      playlists: [],
      error: null,
    }
  }
}
