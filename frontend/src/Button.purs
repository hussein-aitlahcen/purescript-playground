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

module Button where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type State = Unit

data Query a
  = Click a

data Message = Clicked

button :: forall m. H.Component HH.HTML Query Unit Message m
button =
  H.component
    { initialState
    , render
    , eval
    , receiver
    }
  where
    initialState :: Unit -> State
    initialState = const unit

    receiver :: Unit -> Maybe (Query Unit)
    receiver = const Nothing

    render :: State -> H.ComponentHTML Query
    render _ =
      let
        label = "Load user repositories."
      in
        HH.button
          [ HP.title label
          , HE.onClick (HE.input_ Click)
          ]
          [ HH.text label ]

    eval :: Query ~> H.ComponentDSL State Query Message m
    eval (Click next) = H.raise Clicked *> pure next
