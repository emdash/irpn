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

||| High-level calculator interface
module Calc

import Common
import Input
import State

%default total

||| Complete application state
public export
record Calc where
  constructor MkCalc
  state   : State
  history : List State
  undone  : List State
  showing : String

||| User Input supported by the calculator.
public export
data Action
  = Key    Input.Key
  | Apply  String
  | Enter
  | Show   String
  | Define
  | Undo
  | Redo
  | Reset

public export
Eq Action where
  (==) (Key   x)  (Key   y)  = x == y
  (==) (Apply x)  (Apply y)  = x == y
  (==) Enter      Enter      = True
  (==) (Show  x)  (Show y)   = x == y
  (==) Define     Define     = True
  (==) Undo       Undo       = True
  (==) Redo       Redo       = True
  (==) Reset      Reset      = True
  (==) _          _          = False

||| Try to perform the given operation
|||
||| If the operation succeeds:
|||  - update the current sate
|||  - push old state onto the undo stack
|||  - clear redo stack.
||| If the operation fails: return error message
tryOperation
  :  (State -> Either Error State)
  -> Calc
  -> Either Error Calc
tryOperation fn calc = do
  next <- fn calc.state
  pure ({
    state   := next,
    history $= (calc.state ::),
    undone  := []
  } calc)

||| Try to restore the previous state
undo : Calc -> Either Error Calc
undo calc = case calc.history of
  []       => Left  "History is empty"
  x :: xs  => Right ({
    state   := x,
    history := xs,
    undone  $= (calc.state ::)
  } calc)

||| Try to redo an undone action
redo : Calc -> Either Error Calc
redo calc = case calc.undone of
  []      => Left "Redo stack is empty"
  x :: xs => Right ({
    state   := x,
    history $= (calc.state ::),
    undone  := xs
  } calc)

show : String -> Calc -> Either Error Calc
show layout calc = Right ({ showing := layout } calc)


||| Return an empty calculator state
public export
new : Calc
new = MkCalc init [] [] "basic"

||| Try to handle an input event
public export
onEvent : Action -> Calc -> Either Error Calc
onEvent (Key    x) c = tryOperation (input x)  c
onEvent (Apply fn) c = tryOperation (apply fn) c
onEvent Enter      c = tryOperation enter      c
onEvent Define     c = tryOperation define     c
onEvent (Show s)   c = show s                  c
onEvent Undo       c = undo                    c
onEvent Redo       c = redo                    c
onEvent Reset      _ = Right new

test : SnocList Action -> Either Error Calc
test Lin        = Right new
test (events :< event) = do
  pred <- (test events)
  onEvent event pred
