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

module Frame where

import Button as Button
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Prelude

type FrameEffect eff = Aff (ajax :: AX.AJAX | eff)

type State = { loading :: Boolean
             , username :: String
             }

data Query a
  = HandleButton Button.Message a
  | IsLoading (Boolean -> a)
  | UpdateUsername String a

data Message = LoadComplete

data Slot = ButtonSlot

-- Required
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

mainFrame :: forall eff. H.Component HH.HTML Query Unit Void (FrameEffect eff)
mainFrame =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver
    }
  where
    initialState :: Unit -> State
    initialState = const { loading: false
                         , username: ""
                         }

    receiver :: Unit -> Maybe (Query Unit)
    receiver = const Nothing

    render :: State -> H.ParentHTML Query Button.Query Slot (FrameEffect eff)
    render ({ loading: true }) =
      HH.div_
        [ HH.text "Loading repositories..."
        ]
    render state =
      HH.div_
        [ HH.label
            [ HP.for "github-username" ]
            [ HH.text "Github Username" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id_ "github-username"
            , HP.autofocus true
            , HP.value state.username
            , HE.onValueInput (HE.input UpdateUsername)
            ]
        , HH.slot ButtonSlot Button.button unit (HE.input HandleButton)
        ]


    eval :: Query ~> H.ParentDSL State Query Button.Query Slot Void (FrameEffect eff)
    eval (IsLoading reply) = reply <$> H.gets _.loading
    eval (UpdateUsername user next) = H.modify (_ { username = user }) *> pure next
    eval (HandleButton Button.Clicked next) = do
      pure next
      where
        getRepositoriesUrl :: String -> String
        getRepositoriesUrl user = "https://api.github.com/users/" <> user <> "/repos"
