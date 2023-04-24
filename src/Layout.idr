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

||| A DSL for on-screen calculator keypad layouts
module Layout

import Data.List
import Data.Vect

import Calc
import Input
import State

%default total


||| A 2D Grid of values, where some cells may be empty.
public export
data Grid
  :  (rows: Nat)
  -> (cols: Nat)
  -> (cell: Type)
  -> Type
  where
    MkGrid :  Vect (rows * cols) cell -> Grid rows cols cell

||| Create an empty 2D Grid
public export
init
  :  {rows : Nat}
  -> {cols : Nat}
  -> {ty   : Type}
  -> ty
  -> Grid rows cols ty
init value = MkGrid $ replicate (rows * cols) value

||| Create a 2D grid from a flat vector of the right length
public export
grid
  :  {cell : Type}
  -> {rows : Nat}
  -> {cols : Nat}
  -> Vect (rows * cols) cell
  -> Grid rows cols cell
grid cells = MkGrid cells

||| Project the grid as a nested vector
public export
rows
  :  {rows : Nat}
  -> {cols : Nat}
  -> Grid rows cols cell
  -> Vect rows (Vect cols cell)
rows (MkGrid cells) = kSplits rows cols cells

||| Project the grid into a flat vector
public export
cells
  :  {rows : Nat}
  -> {cols : Nat}
  -> Grid rows cols cell
  -> Vect (rows * cols) cell
cells (MkGrid cs) = cs

||| Get the de-duplicated list of cells
public export
uniqueCells
  :  Eq cell
  => {rows : Nat}
  -> {cols : Nat}
  -> Grid rows cols cell
  -> List cell
uniqueCells (MkGrid cells) =
  let cells = toList cells
      dedup = foldl (\acc, a => union acc [a]) [] cells
  in dedup

public export
Functor (Grid r c) where
  map fn (MkGrid c) = MkGrid (map fn c)


{- Layout DSL -}

||| A layout is a named grid of actions of some fixed size.
public export
data Layout : Type where
  MkLayout : String -> (r: Nat) -> (c: Nat) -> (Grid r c (Maybe Action)) -> Layout

||| A short-hand ADT for constructing actions
|||
||| The goal is to represent keyboard layouts in concise tables.
data Binding
  = O
  | D Digit
  | C Char
  | F String
  | K Key
  | A Action
  | Def
  | Enter
  | Point
  | Frac

bind : Binding -> Maybe Action
bind O        = Nothing
bind (D d)    = Just $ Key $ Dig d
bind (C c)    = Just $ Key $ Alpha c
bind (F f)    = Just $ Apply f
bind (K k)    = Just $ Key k
bind (A a)    = Just a
bind Def      = Just $ Define
bind Enter    = Just $ Enter
bind Point    = Just $ Key Input.Point
bind Frac     = Just $ Key Input.Frac

||| Create a layout from a list of actions
export
layout
  :  String
  -> (r : Nat)
  -> (c : Nat)
  -> Vect (r * c) (Binding)
  -> Layout
layout name rows cols cells =
  MkLayout name rows cols (grid (map bind cells))

export
name : Layout -> String
name (MkLayout n _ _ _) = n

export
height : Layout -> Nat
height (MkLayout _ r _ _) = r

export
width : Layout -> Nat
width (MkLayout _ _ c _) = c

export
actions : (l : Layout) -> List (List (Maybe Action))
actions (MkLayout _ _ _ a) = toList (map toList (rows a))

export
uniqueActions : (l : Layout) -> List Action
uniqueActions (MkLayout _ _ _ a) = catMaybes $ uniqueCells $ a

{- Layouts -}

export
basic : Layout
basic = layout "basic" 5 4 [
  F "swap" , F "div" , F "mul" ,  F "sub" ,
  D Seven  , D Eight , D Nine  ,  F "add" ,
  D Four   , D Five  , D Six   ,  F "add" ,
  D One    , D Two   , D Three ,  A Enter ,
  D Zero   , D Zero  , K Point ,  A Enter
]

export
scientific : Layout
scientific = layout "scientific" 12 4 [
  F "sin"  , F "cos"  , F "tan"    , A Define  ,
  F "log"  , F "ln"   , C 'x'      , C 'y'     ,
  F "pow"  , F "exp"  , F "square" , F "sqrt"  ,
  F "swap" , F "div"  , F "mul"    , F "sub"   ,
  D Seven  , D Eight  , D Nine     , F "add"   ,
  D Seven  , D Eight  , D Nine     , F "add"   ,
  D Four   , D Five   , D Six      , F "add"   ,
  D Four   , D Five   , D Six      , F "add"   ,
  D One    , D Two    , D Three    , A Enter   ,
  D One    , D Two    , D Three    , A Enter   ,
  D Zero   , D Zero   , K Point    , A Enter   ,
  D Zero   , D Zero   , K Point    , A Enter
]

export
fractions : Layout
fractions = layout "frac" 11 4 [
  F "f2"    , F "f4"     , F "f8"      , F "f16"  ,
  F "toFloat" , F "inv"   , F "approx"  , F "toFrac" ,
  F "swap"  , F "div"   , F "mul"    , F "sub" ,
  D Seven   , D Eight    , D Nine      , F "add" ,
  D Seven   , D Eight    , D Nine      , F "add" ,
  D Four    , D Five     , D Six       , F "add" ,
  D Four    , D Five     , D Six       , F "add" ,
  D One     , D Two      , D Three     , A Enter  ,
  D One     , D Two      , D Three     , A Enter  ,
  D Zero    , D Zero     , K Frac      , A Enter  ,
  D Zero    , D Zero     , K Frac      , A Enter
]

export
lQwerty : Layout
lQwerty = layout "a" 5 10 [
  D One , D Two , D Three , D Four , D Five , D Six , D Seven , D Eight , D Nine , D Zero ,
  C 'q' , C 'w' , C 'e'   , C 'r'  , C 't'  , C 'y' , C 'u'   , C 'i'   , C 'o'  , C 'p'  ,
  O     , C 'a' , C 's'   , C 'd'  , C 'f'  , C 'g' , C 'h'   , C 'j'   , C 'k'  , C 'l'  ,
  O     , O     , C 'z'   , C 'x'  , C 'c'  , C 'v' , C 'b'   , C 'n'   , C 'm'  , O      ,
  O     , O     , Def     , Def    , Enter  , Enter , Point   , Frac    , O      , O
]

export
uQwerty : Layout
uQwerty = layout "A" 5 10 [
  D One , D Two , D Three , D Four , D Five , D Six , D Seven , D Eight , D Nine , D Zero ,
  C 'Q' , C 'W' , C 'E'   , C 'R'  , C 'T'  , C 'Y' , C 'U'   , C 'I'   , C 'O'  , C 'P'  ,
  O     , C 'A' , C 'S'   , C 'D'  , C 'F'  , C 'G' , C 'H'   , C 'J'   , C 'K'  , C 'L'  ,
  O     , O     , C 'Z'   , C 'X'  , C 'C'  , C 'V' , C 'B'   , C 'N'   , C 'M'  , O      ,
  O     , O     , Def     , Def    , Enter  , Enter , Point   , Frac    , O      , O
]

export
layouts : List Layout
layouts = [basic, scientific, fractions, lQwerty, uQwerty]

export
getLayout : String -> Maybe Layout
getLayout "basic" = Just basic
getLayout "scientific" = Just scientific
getLayout "frac" = Just fractions
getLayout "a" = Just lQwerty
getLayout "A" = Just uQwerty
getLayout _  = Nothing
