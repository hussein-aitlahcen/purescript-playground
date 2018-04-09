-- Main.purs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

module Main where

import Prelude

import Cache (CACHE)
import Cache as Cache
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe)
import Fetch (FETCH, Request, Response, fetch, requestURL)
import GlobalScope.Service (onInstall, onFetch, caches)
import Workers (WORKER)

main
  :: forall e
  .  Eff (worker :: WORKER | e) Unit
main =
  preCache *> fromCache

cacheName :: String
cacheName =
  "feed-cache"

preCache
  :: forall e
  .  Eff (worker :: WORKER | e) Unit
preCache = onInstall $ do
  log "Service worker installed"
  storage <- liftEff caches
  cache   <- Cache.openCache storage cacheName
  Cache.addAll cache
    [ "./index.html"
    , "./initiate.js"
    ]

fromCache
  :: forall e
  .  Eff (worker :: WORKER | e) Unit
fromCache = onFetch respondWith waitUntil
  where
    respondWith
      :: forall e'
      .  Request
      -> Aff (worker :: WORKER, cache :: CACHE, console :: CONSOLE | e') (Maybe Response)
    respondWith req = do
      log "Service worker responding to fetch request"
      storage <- liftEff caches
      cache   <- Cache.openCache storage cacheName
      Cache.match cache (requestURL req)

    waitUntil
      :: forall e''
      .  Request
      -> Aff (worker :: WORKER, fetch :: FETCH, cache :: CACHE | e'') Unit
    waitUntil req = do
      storage <- liftEff caches
      cache   <- Cache.openCache storage cacheName
      res     <- fetch req
      Cache.put cache (requestURL req) res
