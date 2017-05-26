package controllers

import "github.com/chances/chances-party/cache"

type Controller struct {
	Cache cache.Store
}

func (cr *Controller) Setup(cacheStore cache.Store) {
	cr.Cache = cacheStore
}
