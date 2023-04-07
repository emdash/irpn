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


app : JSIO ()
app = do
  Just root <- getElementById !document "state" | Nothing => pure ()
  foo       <- createElement' !document "div"
  ignore $ root `replaceChildren` [Z (foo :> Node)]
  pure ()

main : IO ()
main = runJS app

{-
import Calc
import Input
import Render
  let 
    update : Key -> JSIO ()
    update key := do
      innerHTML root .= ""
      
      case enterKey key accum of
        Nothing    => !window.alert "ouch"
        Just accum => app accum
   
   root `appendChild` "1" (key (update (Dig 1)))
   root `appendChild` "2" (key (update (Dig 2)))
   root `appendChild` "/" (key (update Frac))
   -}
