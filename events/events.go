package events

import (
	"time"

	"github.com/dustin/go-broadcast"
)

var eventChannels = make(map[string]broadcast.Broadcaster)
var heartbeatTickers = make(map[*chan interface{}]*time.Ticker)

// Listen to a broadcast channel for messages
func Listen(channel string) chan interface{} {
	listener := make(chan interface{})
	broadcaster := Event(channel)
	broadcaster.Register(listener)
	// Send heartbeat messages to listeners in production
	heartbeat(listener, broadcaster)
	return listener
}

// StopListening to a broadcast channel
func StopListening(channel string, listener chan interface{}) {
	broadcaster := Event(channel)
	heartbeat(listener, broadcaster).Stop()
	broadcaster.Unregister(listener)
	close(listener)
}

// DeleteBroadcast deletes a broadcast channel
func DeleteBroadcast(channel string) {
	b, ok := eventChannels[channel]
	if ok {
		b.Close()
		delete(eventChannels, channel)
	}
}

// Event gets an event Broadcaster to submit events to
func Event(channel string) broadcast.Broadcaster {
	b, ok := eventChannels[channel]
	if !ok {
		b = broadcast.NewBroadcaster(1)
		eventChannels[channel] = b
	}
	return b
}

func heartbeat(listener chan interface{}, broadcaster broadcast.Broadcaster) *time.Ticker {
	// Workaround for https://devcenter.heroku.com/articles/limits#http-timeouts
	//
	// Broadcast a heartbeat message to listeners every 40 seconds to keep
	//  55 second rolling request timeout window open
	ticker, ok := heartbeatTickers[&listener]
	if !ok {
		ticker = time.NewTicker(time.Second * 40)
		heartbeatTickers[&listener] = ticker

		go func() {
			broadcaster.Submit("heartbeat")
			<-ticker.C
		}()
	}
	return ticker
}
