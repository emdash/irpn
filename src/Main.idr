{-
  (c) 2023 Brandon Lewis
  
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
import Common
import Calc
import Input
import Render


mutual    
  ||| Render the dom for the given calculator state
  |||
  ||| Rendering is affected by whether the previous operation resulted
  ||| in an error.
  |||
  ||| Despite teh VDom layer, I don't do any dom diffing or make any
  ||| effort to optimize dom updates. It just re-renders the entire
  ||| tree with each state change.
  render : Calc -> Maybe Error -> JSIO ()
  render calc err = do
    Just root <- getElementById !document "state" | Nothing => consoleLog "WTF"
    -- note that we're capturing calc here in the `doAction` closure.
    contents  <- vrender (doAction calc) (render_calc calc err)
    ignore $ replaceWith root [inject $ contents :> Node]
        
  ||| Helper for binding actions in the virtual dom
  |||
  ||| This has to be defined here on account of the mutual recursion
  ||| between update and render.
  |||
  ||| XXX: look for other ways to factor this?
  |||
  ||| Basically, this will trigger an update and re-render in response
  ||| to DOM events, which get translated to calculator events through
  ||| the magic of the virtual dom.
  doAction : Calc -> Calc.Event -> ignored -> IO ()
  doAction prev action _ = case Calc.onEvent action prev of
    Left err   => update prev (Just err)
    Right next => update next Nothing

  ||| Update the UI to reflect a change of applicatino state.
  update : Calc -> Maybe Error -> IO ()
  update prev err  = runJS (render prev err)


main : IO ()
main = update new Nothing
