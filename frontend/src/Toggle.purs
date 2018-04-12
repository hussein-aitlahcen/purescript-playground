-- Toggle.purs ---

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

module Toggle
       (
         toggle,
         State,
         Query,
         Message
       )
       where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

toggle :: forall m. H.Component HH.HTML Query Unit Message m
toggle =
  H.component
    { initialState
    , render
    , eval
    , receiver
    }
  where
    initialState :: Unit -> State
    initialState = const false

    receiver :: Unit -> Maybe (Query Unit)
    receiver = const Nothing

    render :: State -> H.ComponentHTML Query
    render state =
      let
        labelOf true  = "On"
        labelOf false = "Off"
        label = labelOf state
      in
        HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Toggle)
          ]
          [ HH.text label ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Toggle next -> do
          nextState <- H.gets not
          H.put nextState
          H.raise (Toggled nextState)
          pure next
      IsOn reply -> reply <$> H.get
