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


||| Render the dom for the given calculator state
|||
||| Rendering is affected by whether the previous operation resulted
||| in an error.
|||
||| I don't do any dom diffing or make any effort to optimize dom
||| updates. We just blow the old DOM subtree away and re render on
||| every state change.
render : Calc -> Maybe Error -> JSIO ()
render calc err = do
  Just root <- getElementById !document "state" | Nothing => consoleLog "fuck!"
  contents  <- vrender (doAction calc) (render_calc calc err)
  ignore $ replaceWith root [inject $ contents :> Node]
  where
    ||| Re-render the UI with the new state
    update : Calc -> Maybe Error -> IO ()
    update state err  = runJS (render state err)
    ||| Binds actions in the VDom to concrete DOM events
    |||
    ||| This gets partially applied above to yield a function from
    ||| Action -> Dom Event -> IO ()
    doAction : Calc -> Calc.Action -> ignored -> IO ()
    doAction prev action _ = case Calc.onEvent action prev of
      Left err   => update prev (Just err)
      Right next => update next Nothing

||| This is the application entry point, and it runs to completion
||| exactly once.
app : JSIO ()
app = do
  consoleLog "in App"
  render new Nothing

main : IO ()
main = runJS app
