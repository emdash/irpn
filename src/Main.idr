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

import Data.String

import Control.Monad.Indexed.State
import JS
import Web.Dom
import Web.Html
import Web.Internal.DomPrim
import Web.Raw.UIEvents
import Calc
import Input
import Render






renderAccum : Accum -> JSIO Div
renderAccum accum = do
  ret     <- createElement Div
  content <- createElement Div
  ignore $ appendChild ret content
  pure ret


export
render : Div -> Calc -> JSIO ()
render element calc = do
  innerHTML element .= ""  
  element `appendChild` 
  



main : IO ()
main = runJS (render Calc.new)
