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

||| Contains definitions related to user input and the input accumulator
module Input

import Data.Nat
import Data.String
import Common

%default total

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