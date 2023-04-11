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
  ||| I don't do any dom diffing or make any effort to optimize dom
  ||| updates. The only thing we do here is make sure we construct the
  ||| entire tree before replacing it wholesale, so every state change
  ||| triggers at most one browser reflow event.
  render : Element -> Calc -> Maybe Error -> JSIO ()
  render root calc err = do
    contents  <- vrender (doAction root calc) (render_calc calc err)
    ignore $ replaceWith root [inject $ contents :> Node]
        
  ||| Helper for binding actions in the virtual dom
  |||
  ||| This has to be defined here on account of the mutual recursion
  ||| between update and render.
  |||
  ||| XXX: this feels slightly wrong to me.
  |||
  ||| Basically, this will update and re-render in response to DOM
  ||| events, which get translated to calculator events through the
  ||| magic of the virtual dom.
  doAction : Element -> Calc -> Calc.Action -> ignored -> IO ()
  doAction root prev action _ = case Calc.onEvent action prev of
    Left err   => update root prev (Just err)
    Right next => update root next Nothing

  ||| Update the UI to reflect a change of applicatino state.
  update : Element -> Calc -> Maybe Error -> IO ()
  update root prev err  = runJS (render root prev err)

||| A function we can pass to runJS.
|||
||| We construct the root element, add it to the dom, and then perform
||| the initial render.
|||
||| Alternatively, we could *find* the root element in the dom, if we
||| wanted to rely on some static HTML.
app : JSIO ()
app = do
  root <- createElement !document "div"
  ignore $ appendChild !body root
  render root new Nothing

main : IO ()
main = runJS app
