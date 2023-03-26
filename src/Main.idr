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
|||
||| This is an example of an interactive web application written in
||| idris. It is also meant to replace my original calculator.


module Main


import Data.Nat
import Data.HashMap
import Data.String


%default total


||| Type of fractions
data Rat = MkRat Integer Nat

||| Type of all values
data Value 
  = I Integer
  | F Double
  | R Rat
  | S String
  | P (SnocList Value)
  
||| The type of all user-visible error messages
Error : Type
Error = String

||| Translate a maybe value to an Either
fromMaybe : b -> Maybe a -> Either b a
fromMaybe x Nothing  = Left  x
fromMaybe x (Just y) = Right y


namespace Input

  ||| Decimal digits sent to the accumulator
  public export
  data Digit
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine

  ||| All tokens recognized by the accumulator
  public export
  data Key : Type where
    Alpha     : Char     -> Key
    Dig       : Digit    -> Key
    Point     : Key
    Frac      : Key
    Clear     : Key

  ||| Holds and validates user input
  public export
  data Accum : Type where
    Empty   :                                        Accum
    Digits  :                       Integer       -> Accum
    Decimal :            Integer -> Integer       -> Accum
    Num     :            Integer -> Integer       -> Accum
    Denom   : Integer -> Integer -> Maybe Nat     -> Accum
    Id      :                       SnocList Char -> Accum
  
  ||| Fold a digit into an integer value
  foldDigit : Integral a => a -> Digit -> a
  foldDigit i Zero  = i * 10
  foldDigit i One   = i * 10 + 1
  foldDigit i Two   = i * 10 + 2
  foldDigit i Three = i * 10 + 3
  foldDigit i Four  = i * 10 + 4
  foldDigit i Five  = i * 10 + 5
  foldDigit i Six   = i * 10 + 6
  foldDigit i Seven = i * 10 + 7
  foldDigit i Eight = i * 10 + 8
  foldDigit i Nine  = i * 10 + 9
  
  ||| Convert a digit to a character
  digitToChar : Digit -> Char
  digitToChar Zero  = '0'
  digitToChar One   = '1'
  digitToChar Two   = '2'
  digitToChar Three = '3'
  digitToChar Four  = '4'
  digitToChar Five  = '5'
  digitToChar Six   = '6'
  digitToChar Seven = '7'
  digitToChar Eight = '8'
  digitToChar Nine  = '9'

  ||| Store a digit into an accumulator
  |||
  ||| This is always allowed
  enterDigit : Accum -> Digit -> Accum
  enterDigit Empty                   d = Digits      (foldDigit 0  d)
  enterDigit (Digits      ds)        d = Digits      (foldDigit ds d)
  enterDigit (Decimal i   ds)        d = Decimal i   (foldDigit ds d)
  enterDigit (Num     i   ds)        d = Num     i   (foldDigit ds d)
  enterDigit (Denom   i n Nothing)   d = Denom   i n (Just (foldDigit 0  d))
  enterDigit (Denom   i n (Just ds)) d = Denom   i n (Just (foldDigit ds d))
  enterDigit (Id          s)         d = Id          (s :< (digitToChar d))

  ||| Store a letter into an accumulator
  |||
  ||| This is only allowed when the accumulator is empty or already
  ||| contains chars.
  enterAlpha : Accum -> Char -> Maybe Accum
  enterAlpha Empty  c = Just (Id (Lin :< c))
  enterAlpha (Id s) c = Just (Id (s   :< c))
  enterAlpha _      _ = Nothing
  
  ||| Send the decimal point to the accumulator
  |||
  ||| This is only allowed when the accumulator is empty or contains digits.
  enterPoint : Accum -> Maybe Accum
  enterPoint Empty      = Just (Decimal 0 0)
  enterPoint (Digits i) = Just (Decimal i 0)
  enterPoint _          = Nothing
  
  ||| Send the faction symbol to the accumulator
  |||
  ||| This is allowed if the accumulator:
  ||| - is empty
  ||| - contains only digits
  ||| - contains an incomplete fraction
  enterFrac : Accum -> Maybe Accum
  enterFrac Empty         = Just (Num 0 0)
  enterFrac (Digits  i)   = Just (Num i 0)
  enterFrac (Decimal _ _) = Nothing
  enterFrac (Num     i n) = Just (Denom i n Nothing)
  enterFrac (Denom _ _ _) = Nothing
  enterFrac (Id _)        = Nothing
  
  ||| Send a symbol to the accumulator
  |||
  ||| If the symbol is accepted, the new accumulator state is
  ||| returned. If the symbol is rejected, Nothing is returned.
  public export
  enterKey : Accum -> Key -> Maybe Accum
  enterKey accum (Alpha a) = enterAlpha accum a
  enterKey accum (Dig   d) = Just (enterDigit accum d)
  enterKey accum Point     = enterPoint accum
  enterKey accum Frac      = enterFrac  accum
  enterKey _     Clear     = Just Empty
    
  ||| Get the value from the accumulator
  |||
  ||| This is only possible when the accumulator contains a complete value.
  public export
  value : Accum -> Either Error Value
  value Empty                = Left "Accumulator is empty."
  value (Digits i)           = Right  (I i)
  value (Decimal i j)        = case parseDouble ((show i) ++ "." ++ (show j)) of
    Nothing                  => Left "Invalid Double"
    Just f                   => Right (F f)
  value (Num i n)            = Left "Incomplete fraction."
  value (Denom i n Nothing)  = Left "Incomplete fraction."
  value (Denom i n (Just d)) = Right  (R (MkRat ((i * (cast d)) + n) d))
  value (Id id)              = Right (S (pack (asList id)))


||| The type of a function in this calculator
|||
||| Functions are conceptually from Stack -> Stack, but may fail if
||| the stack configuration is invalid
StackFn : Type
StackFn = List Value -> Either Error (List Value)

||| A stack function which always pushes a value
constFn : Value -> StackFn
constFn x xs = Right (x :: xs)

||| Placeholder for unimplemented functions
unimplemented : String -> StackFn
unimplemented name xs = Left ("Unimplemented: " ++ name)

||| A high-level input token 
public export
data Token = Val Value | Fn String

||| Maps names to calculator functions at runtime.
Env : Type
Env = HashMap String StackFn

||| Table of builtin functions
builtins : Env
builtins = fromList [
  ("pi" , constFn (F 3.14159)),
  ("e"  , constFn (F 2.7178)),
  ("+"  , unimplemented "+"),
  ("-"  , unimplemented "-"),
  ("*"  , unimplemented "*"),
  ("/"  , unimplemented "/")
]

||| Wrapper around hashtable lookup which translates errors
lookup : String -> Env -> Either Error StackFn
lookup key env = case HashMap.lookup key env of
  Nothing => Left "Invalid Function"
  Just f  => Right f

||| State of calculations at a given time
public export
record State where
  constructor MkState
  accum : Input.Accum
  stack : List Value
  tape  : SnocList Token
  env   : Env
  
||| Initial state of the calculator
init : State
init = MkState Empty [] Lin builtins
  
||| Send an input symbol to the accumulator
public export
input : Input.Key -> State -> Either Error State
input symbol state = do
  next <- fromMaybe "Invalid Char" (enterKey (accum state) symbol)
  pure ({ accum := next } state)

||| Transfer a value from accumulator to stack
|||
||| The accumulator will be cleared, and the value appended to the tape.
public export
enter : State -> Either Error State
enter state = do
  value <- Input.value (accum (state))
  pure ({ 
      accum := Empty,
      stack $= (value ::),
      tape  $= (:< (Val value))
  } state)

||| Return the top of stack, within the either monad
public export
top : State -> Either Error Value
top state = case (stack state) of
  []      => Left "Stack Underflow"
  x :: xs => Right x

||| Remove and return the top of the stack, within the either monad
public export
pop : State -> Either Error (Value, State)
pop state = case (stack state) of
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
  state <- enter state
  func  <- lookup name (env state)
  stack <- func (stack (state))
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

||| Complete application state
public export
record Calc where
  constructor MkCalc
  state   : State
  history : List State
  undone  : List State

||| User Input supported by the calculator.
public export
data Event
  = Key    Input.Key 
  | Apply  String
  | Enter
  | Define
  | Undo
  | Redo
  | Reset


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
  next <- fn (state calc)
  pure ({
    state   := next,
    history $= ((state calc) ::),
    undone  := []
  } calc)
  
  
||| Try to restore the previous state
undo : Calc -> Either Error Calc
undo calc = case history calc of
  []      => Left  "History is empty"
  x :: xs => Right ({
    state   := x, 
    history := xs, 
    undone  $= (x ::) 
  } calc)


||| Try to redo an undone action
redo : Calc -> Either Error Calc
redo calc = case undone calc of
  []      => Left "Redo stack is empty"
  x :: xs => Right ({
    state   := x,
    history $= (x ::),
    undone  := xs
  } calc)
    

||| Try to handle an input event
public export
onEvent : Event -> Calc -> Either Error Calc
onEvent (Key    x) c = tryOperation (input x)  c
onEvent (Apply fn) c = tryOperation (apply fn) c
onEvent Enter      c = tryOperation enter      c
onEvent Define     c = tryOperation define     c
onEvent Undo       c = undo c
onEvent Redo       c = redo c
onEvent Reset      _ = Right (MkCalc init [] [])
