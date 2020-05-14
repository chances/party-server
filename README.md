# [Chance's Party App](http://chancesnow.me/party)

[![Build Status](https://img.shields.io/github/workflow/status/chances/party-server/Tunage%20CI)](https://github.com/chances/party-server/actions)

A party web app to facilitate guest interaction at your parties.

This application tightly integrates with the [Spotify Web API](https://developer.spotify.com/web-api/) to help party hosts and guests curate a playlist of music as well as keep a guest list for a party.

## Running

1. Make a copy of `.env.example` as `.env` and replace necessary values.
2. Run `make` in the root directory
4. Ensure Postgres is running, bootstrapped with [party.sql](party.sql)
2. Ensure Redis is running
3. Run `make run`

## Contributing

Feel free to contribute.

1. Inspect the [TODO.md](TODO.md) document for new work.

2. Pick something farther down the list because I'm working top-down and have likely already started on those.

3. Read the [CONTRIBUTING.md](CONTRIBUTING.md) document for further instructions.

## License

[MIT License](http://opensource.org/licenses/MIT)

Copyright &copy; 2016-2020 Chance Snow. All rights reserved.
