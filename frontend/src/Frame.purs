-- Frame.purs ---

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

module Frame
       (
         mainFrame,
         Query,
         Message
       )
       where

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude
import Toggle as T

type State = { loading :: Boolean }

data Query a
  = HandleToggle T.Message a
  | IsLoaded (Boolean -> a)

data Message = Loaded

data Slot = ToggleSlot

-- Required
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

mainFrame :: forall m. H.Component HH.HTML Query Unit Void m
mainFrame =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver
    }
  where
    initialState :: Unit -> State
    initialState i = { loading: false }

    receiver :: Unit -> Maybe (Query Unit)
    receiver = const Nothing

    render :: State -> H.ParentHTML Query T.Query Slot m
    render state =
      HH.div_
        [ HH.p_
            [ HH.text (stateText state.loading)
            ]
        , HH.slot ToggleSlot T.toggle unit (HE.input HandleToggle)
        ]
      where
        stateText false = "Loading..."
        stateText true = "Loading complete ! :D"

    eval :: Query ~> H.ParentDSL State Query T.Query Slot Void m
    eval = case _ of
      IsLoaded reply -> reply <$> H.gets _.loading
      HandleToggle _ next -> do
        H.put { loading: true }
        pure next
