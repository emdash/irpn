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

||| A Small DSL for defining calculator keypad layouts
module Layout
import State
import Calc


data Item
  = Symbol        String 
  | Func
  | Unimplemented 


import Data.Vect
import Calc


public export
data Grid 
  :  (rows: Nat) 
  -> (cols: Nat) 
  -> (cell: Type) 
  -> Type 
 where
   MkGrid 
    :  Vect rows (Vect cols (Maybe cell)) 
    -> Grid rows cols cell


public export
blank 
  :  {rows : Nat}
  -> {cols : Nat}
  -> {ty   : Type}
  -> Grid rows cols ty
blank {rows} {cols} = MkGrid $ replicate rows $ replicate cols Nothing


public export
assign 
  :  Grid r c cell 
  -> Fin r 
  -> Fin c 
  -> cell 
  -> Grid r c cell
assign (MkGrid rows) row col value =
  let 
    rowv = index row rows
    new  = replaceAt col (Just value) rowv
  in
    MkGrid (replaceAt row new rows)

public export
record Layout where
  constructor MkLayout
  name: String
  keys: Grid rows cols String


public export
layout : String -> Vect n String -> Maybe Layout
