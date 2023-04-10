{-
  (c) 2021 Brandon Lewis
  
  This file is part of irpn.
  
  rpncalc is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.
  
  rpncalc is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Affero General Public License for more details.
  
  You should have received a copy of the GNU Affero General Public License
  along with rpncalc.  If not, see <https://www.gnu.org/licenses/>.
-}

||| This is an idris port of my JavaScript RPN Calculator
module Main

import Control.Monad.Either
import Data.String
import JS
import Web.Internal.Types
import Web.Raw.Dom
import Web.Dom
import Calc
import Input
import Render


mutual    
  ||| Render the dom for the given calculator state
  render : Calc -> (Maybe Calc -> IO ()) -> JSIO ()
  render calc update = do
    -- get the root element from the dom
    Just root <- getElementById !document "state" | Nothing => consoleLog "WTF"
    
    -- render the new contents
    contents  <- vrender (render_calc calc Nothing)
 
    -- replace the old contents with the new contents
    ignore $ replaceWith root [inject $ contents :> Node]
    

  ||| Update the application state
  update : Maybe Calc -> IO ()
  update Nothing     = do pure ()
  update (Just calc) = do runJS (render calc update)


main : IO ()
main = update (Just new)
