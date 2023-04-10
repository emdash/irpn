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

||| High level wrapper for binding event listeners
|||
||| The low level addEventListener really requires way too much
||| boilerplate, which I am hiding away here. The attributes interface
||| is too high-level / complicated for where I'm at right now.
on : Element -> String -> (Types.Event -> IO ()) -> JSIO ()
on element event handler = do
  handler <- toEventListener handler
  ignore $ addEventListener element event (Just handler)

mutual
  onclick : Accum -> String -> Key -> Types.Event -> IO ()
  onclick accum msg k _ = do
    consoleLog msg
    update (enterKey k accum)

  key : Accum -> String -> Key -> JSIO Element
  key accum label k = do
    ret <- createElement  !document "button"
    txt <- createTextNode !document label
    ignore $ appendChild      ret txt
    on ret "click" (onclick accum "foo" k)
    pure ret
    
    
  ||| Re-render the application state in JSIO
  render : Accum -> JSIO ()
  render accum = do
    -- get the root element from the dom
    Just root <- getElementById !document "state" | Nothing => consoleLog "WTF"
    
    -- render the new contents
    contents  <- vrender (render_accum accum Nothing)

    -- replace the old contents with the new contents
    ignore $ replaceChildren root [inject $ contents :> Node]
    
    -- xxx: this is temporary
    ignore $ appendChild     root !(key accum "1" (Dig One))
    ignore $ appendChild     root !(key accum "2" (Dig Two))
    ignore $ appendChild     root !(key accum "/" Frac)
    

  ||| Update the application state
  update : Maybe Accum -> IO ()
  update Nothing      = do pure ()
  update (Just accum) = do runJS (render accum)


main : IO ()
main = update (Just Empty)
