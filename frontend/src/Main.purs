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

import Control.Monad.Eff (Eff)
import Frame as F
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff (runUI F.mainFrame unit =<< HA.awaitBody)
