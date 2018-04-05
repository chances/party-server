import { Component } from '@nestjs/common'
import * as fs from 'fs'
import ntml from 'lit-ntml'
import * as path from 'path'
import { AuthView, IndexView } from './models/views'

export function engine(service: ViewService) {
  return (filePath: string, options: any,
          callback: (err: NodeJS.ErrnoException | null, rendered?: string) => void) => {
    const basename = path.basename(filePath, '.lit')
    if (basename === 'index' || basename === 'auth') {
      service.index(options).then(dynamicContent => {
        fs.readFile(filePath, async (err, content) => {
          if (err) { return callback(err) }
          return callback(
            null,
            content.toString().replace('<content></content>', dynamicContent),
          )
        })
      }).catch(err => {
        callback(err)
      })
    } else {
      fs.readFile(filePath, async (err, content) => {
        if (err) { return callback(err) }
        return callback(null, content.toString())
      })
    }
  }
}

@Component()
export class ViewService {
  private readonly html = ntml({
    minify: true,
    parseHtml: false,
  })

  async index({user, currentParty, currentPlaylist, playlists, error}: IndexView) {
    const currentPlaylistName = currentPlaylist != null
      ? currentPlaylist.name
      : 'N/A'

    if (user == null) {
      return this.html`<p>
        <a href="/auth/login?return_to=%2F">Login with Spotify</a>
      </p>`
    } else {
      const currentPartyDetails = currentParty != null
        ? `<div class="details">
        <p>Current party: <span id="currentParty">{{ .currentParty.RoomCode }}</span></p>
      </div>`
        : currentPlaylist != null ? `<p>
          <a id="startParty"
            data-id="${currentPlaylist.id}"
            data-host="${user.displayName}" href="#party/start">Start Party</a>
        </p>` : ''
      const errorMessage = error != null
        ? `<p>Error:</p><p>${error}</p>`
        : ''

      return this.html`<p>
        <a href="/auth/logout">Log Out</a>
      </p>
      <h1>Logged in as ${user.displayName}</h1>
      <img id="avatar" width="200" height="200" src="${user.images[0]}" />
      <div id="user" class="details">
        <p>ID: ${user.id}</p>
        <p>Profile: <a href="${user.uri}">${user.uri}</a></p>
        <p>Followers: ${user.followers.length}</p>
        <p>Country: ${user.country}</p>
        <p>Product: ${user.product}</p>
      </div>
      <p>Current playlist: <span id="currentPlaylist">
        ${currentPlaylistName}
      </span></p>
      ${currentPartyDetails}
      <div style="clear: both;"></div>
      ${
        playlists != null
          ? '<h2>Pick a playlist for the party</h2>'
          : ''
      }
      ${
        playlists != null
          ? playlists.filter(playlist => playlist.owner === user.id).map(this.playlist)
          : ''
      }
      ${errorMessage}`
    }
  }

  auth({username, host}: AuthView) {
    if (username == null) {
      // tslint:disable:max-line-length
      return this.html`
<span class="spinner">
    <svg
        x="0px"
        y="0px"
        width="60px"
        height="60px"
        viewBox="0 0 50 50"
        style="enable-background:new 0 0 50 50;"
    >
      <path
        class="spinner"
        fill="#FCFCFC"
        d="M25.251,6.461c-10.318,0-18.683,8.365-18.683,18.683h4.068c0-8.071,6.543-14.615,14.615-14.615V6.461z"
        />
    </svg>
</span>
<script type="application/javascript">
  window.addEventListener('load', function loaded(e) {
    setTimeout(function redirect() {
      window.location.replace('//{{ .host }}/auth/login');
    }, 500);
  });
    </script>`
    } else {
      return this.html`<p>Logged in as ${username}</p>`
    }
  }

  private playlist(playlist: {id: number, name: string, endpoint: string}) {
    return `<li>
    <a class="playlist" data-id="${playlist.id}" href="#playlist/${playlist.id}">
      ${playlist.name}
    </a>
    <a href="${playlist.endpoint }" class="link-back">On Spotify</a>
  </li>`
  }
}
