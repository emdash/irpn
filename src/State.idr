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

||| This module implements the high-level calculator functions
module State

import Data.HashMap
import Common
import Input

%default total

||| The type of a function in this calculator
|||
||| Functions are from Stack -> Stack, and may fail.
StackFn : Type
StackFn = List Value -> Either Error (List Value)

||| Exchange the top two stack values
swap : StackFn
swap (x :: y :: xs) = Right $ y :: x :: xs
swap _              = Left "Stack underflow"

||| A stack function which always pushes a constant value
constFn : Value -> StackFn
constFn x xs = Right (x :: xs)

||| A stack function which wraps a unary function
unaryFn : (Value -> Either Error Value) -> StackFn
unaryFn f (x :: xs) = map (:: xs) (f x)
unaryFn _ _         = Left "Stack underflow"

||| Wraps builtin functions on Double.
|||
||| We support integer operations by casting through Double.
doubleFn : (Double -> Double) -> Value -> Either Error Value
doubleFn f (I i)   = Right $ I $ cast $ f $ cast i
doubleFn f (F dbl) = Right $ F $ f dbl
doubleFn f _       = Left "Invalid Operand"

||| A stack function which wraps a binary operation
binaryFn : (Value -> Value -> Either Error Value) -> StackFn
binaryFn f (y :: x :: xs) = map (:: xs) (f x y)
binaryFn _ _              = Left "Stack underflow"

add : Value -> Value -> Either Error Value
add (I x) (I y) = Right $ I $ x + y
add (F x) (F y) = Right $ F $ x + y
add (R x) (R y) = Left "Unimplemented"
add _      _    = Left "Unimplemented"

sub : Value -> Value -> Either Error Value
sub (I x) (I y) = Right $ I $ x - y
sub (F x) (F y) = Right $ F $ x - y
sub (R x) (R y) = Left "Unimplemented"
sub _      _    = Left "Unimplemented"

mul : Value -> Value -> Either Error Value
mul (I x) (I y) = Right $ I $ x * y
mul (F x) (F y) = Right $ F $ x * y
mul (R x) (R y) = Left "Unimplemented"
mul _      _    = Left "Unimplemented"

div : Value -> Value -> Either Error Value
div (I x) (I y) = Right $ I $ x `div` y
div (F x) (F y) = Right $ F $ x / y
div (R x) (R y) = Left "Unimplemented"
div _      _    = Left "Unimplemented"


||| Placeholder for unimplemented functions
unimplemented : String -> StackFn
unimplemented name xs = Left ("Unimplemented: " ++ name)

||| Maps names to calculator functions at runtime.
Env : Type
Env = HashMap String StackFn

||| Table of builtin functions
builtins : Env
builtins = fromList [
  ("pi"      , constFn  (F 3.14159)),
  ("e"       , constFn  (F 2.7178)),
  ("add"     , binaryFn add),
  ("sub"     , binaryFn sub),
  ("mul"     , binaryFn mul),
  ("div"     , binaryFn div),
  ("swap"    , swap),
  ("tanh"    , unaryFn (doubleFn tanh)),
  ("tan"     , unaryFn (doubleFn tan)),
  ("sqrt"    , unaryFn (doubleFn sqrt)),
  ("sinh"    , unaryFn (doubleFn sinh)),
  ("sin"     , unaryFn (doubleFn sin)),
  ("log"     , unaryFn (doubleFn log)),
  ("floor"   , unaryFn (doubleFn floor)),
  ("exp"     , unaryFn (doubleFn exp)),
  ("cosh"    , unaryFn (doubleFn cosh)),
  ("cos"     , unaryFn (doubleFn cos)),
  ("ceil"    , unaryFn (doubleFn ceiling)),
  ("atan"    , unaryFn (doubleFn atan)),
  ("asin"    , unaryFn (doubleFn asin)),
  ("acos"    , unaryFn (doubleFn acos))
]

||| Wrapper around hashtable lookup which translates errors
lookup : String -> Env -> Either Error StackFn
lookup key env = case HashMap.lookup key env of
  Nothing => Left "Invalid Function"
  Just f  => Right f

||| A high-level input token
public export
data Token = Val Value | Fn String

||| State of calculations at a given time
public export
record State where
  constructor MkState
  accum : Input.Accum
  stack : List Value
  tape  : SnocList Token
  env   : Env

||| Initial state of the calculator
public export
init : State
init = MkState Empty [] Lin builtins

||| Send an input symbol to the accumulator
public export
input : Input.Key -> State -> Either Error State
input symbol state = do
  next <- fromMaybe "Invalid Char" (enterKey symbol state.accum)
  pure ({ accum := next } state)

||| Transfer a value from accumulator to stack
|||
||| The accumulator will be cleared, and the value appended to the tape.
public export
enter : State -> Either Error State
enter state = do
  value <- Input.value state.accum
  pure ({
      accum := Empty,
      stack $= (value ::),
      tape  $= (:< (Val value))
  } state)

||| Return the top of stack, within the either monad
public export
top : State -> Either Error Value
top state = case state.stack of
  []      => Left "Stack Underflow"
  x :: xs => Right x

||| Remove and return the top of the stack, within the either monad
public export
pop : State -> Either Error (Value, State)
pop state = case state.stack of
  []      => Left "Stack Underflow"
  x :: xs => Right (x, { stack := xs } state)

||| Try to apply a named function to the current stack.
|||
||| If the accumulator is non-empty, its contents will first be
||| transfered to the stack.
|||
||| If application succeeds, the resulting state is returned.
||| If the stack is in an invalid state, the error will be returned.
public export
apply : String -> State -> Either Error State
apply name state = do
  state <- if isEmpty state.accum
           then (Right state)
           else (enter state)
  func  <- lookup name state.env
  stack <- func state.stack
  pure ({
    stack := stack,
    tape $= (:< Fn name)
  } state)

||| Bind the name at the top of stack to the value immediately below it.
|||
||| This will re-bind the name, if it was previously bound.
public export
define : State -> Either Error State
define state = do
  ((S name), state) <- pop state | _ => Left "Not a String"
  (val, state)      <- pop state
  pure ({
    env $= (HashMap.insert name (constFn val))
  } state)
