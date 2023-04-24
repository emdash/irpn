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


||| Operations on naive fractions
module Rat

%default total

||| Type of fractions
export
record Rat where
  constructor MkRat
  num   : Integer
  denom : Nat

export
Show Rat where
  show (MkRat num denom) = (show num) ++ "/" ++ (show denom)

public export
rat : Integer -> Nat -> Either String Rat
rat num 0     = Left "Zero in Denominator"
rat num denom = Right $ MkRat num denom

public export
unpack : Rat -> (Integer, Nat)
unpack (MkRat num denom) = (num, denom)

public export
add : Rat -> Rat -> Either String Rat
add a b = rat
  (a.num * (cast b.denom) + b.num * (cast a.denom))
  (a.denom * b.denom)

public export
sub : Rat -> Rat -> Either String Rat
sub a b = rat
  (a.num * (cast b.denom) - b.num * (cast a.denom))
  (a.denom * b.denom)

public export
inv : Rat -> Either String Rat
inv (MkRat num denom) = rat (preserveSign num denom) (cast num)
  where
    preserveSign : Integer -> Nat -> Integer
    preserveSign num denom =
      if (num < 0)
      then negate (cast denom)
      else cast denom

public export
mul : Rat -> Rat -> Either String Rat
mul a b = rat (a.num * b.num) (a.denom * b.denom)

public export
div : Rat -> Rat -> Either String Rat
div a b = mul a !(inv b)

public export
pow : Rat -> Integer -> Either String Rat
pow r exp =
  if (exp < 0)
    then pow_int !(inv r) (cast exp)
    else pow_int r        (cast exp)
  where
    pow_int : Rat -> Nat -> Either String Rat
    pow_int x 0     = rat 1 1
    pow_int x (S k) = mul x !(pow_int x k)

public export
approx : Rat -> Nat -> Either String Rat
approx rat denom = Left "Unimplemented"

public export
tryInt : Rat -> Either String Integer
tryInt (MkRat num 1) = Right num
tryInt _             = Left "Not Integral"
