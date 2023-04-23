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

||| Common definitions that don't beling to any other module
module Common

%default total

||| The type of all user-visible error messages
public export
Error : Type
Error = String

||| Type of fractions
public export
data Rat = MkRat Integer Nat

||| Translate a maybe value to an Either
|||
||| XXX: I'm sure there's a standard library function that does this.
public export
fromMaybe : b -> Maybe a -> Either b a
fromMaybe x Nothing  = Left  x
fromMaybe x (Just y) = Right y

||| Combinator for working with maybes
|||
||| XXX: Probably a standard library function that does this.
public export
tryWith : (a -> Maybe b) -> (a -> b) -> a -> b
tryWith try otherwise x = case try x of
  Nothing => otherwise x
  Just y  => y

public export
withDefault : (a -> b) -> b -> Maybe a -> b
withDefault fn def Nothing  = def
withDefault fn def (Just v) = fn v

||| Type of all values
public export
data Value
  = I Integer
  | F Double
  | R Rat
  | S String
  | P (SnocList Value)
