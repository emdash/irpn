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

import Data.Nat
import Data.Nat.Factor


%default total


||| Find the element of xs which minimizes `fn x` over some sequence.
|||
||| XXX: move to Common.idr once circular dependency with Rat is
||| resolved.
public export
argmin : Ord b => Foldable seq => (a -> b) -> a -> seq a -> a
argmin fn init xs =
  let
    acc         = (init, (fn init))
    (result, _) = foldl helper acc xs
  in result
  where
    helper : (a, b) -> a -> (a, b)
    helper prev@(result, least) next =
      let check = fn next
      in case compare check least of
        LT => (next, check)
        EQ => prev
        GT => prev

||| A number represented as the signed ratio of two integers.
export
record Rat where
  constructor MkRat
  -- I could have used some more mathy representation, but the
  -- behavior I want here is to preserve denominators, until the user
  -- explicitly simplifies.
  num   : Integer
  -- dependent types would let us restrict the denominator to be
  -- nonzero, not just positive. But in practice, we have to do
  -- runtime checks for zero after most operations, so I'll take the
  -- easy road here and use the "smart constructor" pattern.
  denom : Nat

  -- we need this to apply gcd in `simplify`
  zeroSafe : NotBothZero (cast num) denom

||| A useful constant
public export
one  : Rat
one = MkRat 1 1 RightIsNotZero

||| Cast a nat to an integer, using the original sign
public export
keepSign : Integer -> Nat -> Integer
keepSign sign value =
  let ret = cast value
  in if (sign < 0)
    then negate ret
    else ret

||| Divide an integer by a nat, preserving original sign
public export
signedDiv : Integer -> Nat -> Integer
signedDiv x y = keepSign x ((cast (abs x)) `div` y)

||| Rational numbers are represented as `num / denom`
export
Show Rat where
  show (MkRat num denom _) = (show num) ++ "/" ++ (show denom)

||| Safe constructor for rationals.
public export
rat : Integer -> Nat -> Either String Rat
rat num 0         = Left "Zero in Denominator"
rat num (S denom) = Right $ MkRat num (S denom) RightIsNotZero

||| Project a tuple of signed numerator and unsigned denominator.
public export
unpack : Rat -> (Integer, Nat)
unpack (MkRat num denom _) = (num, denom)

||| Apply f on the cross-multiplication of a and b
withCrossMul : (Integer -> Integer -> a) -> Rat -> Rat -> a
withCrossMul f a b = f (a.num * cast b.denom) (b.num * cast a.denom)

||| Add two rational numbers
public export
add : Rat -> Rat -> Either String Rat
add a b = rat (withCrossMul (+) a b) (a.denom * b.denom)

||| Subtract two rational numbers
public export
sub : Rat -> Rat -> Either String Rat
sub a b = rat (withCrossMul (-) a b) (a.denom * b.denom)

||| Compute the multiplicative inverse of the given rational number
public export
inv : Rat -> Either String Rat
inv (MkRat num denom _) = rat (keepSign num denom) (cast num)

||| Multiply two rational numbers
public export
mul : Rat -> Rat -> Either String Rat
mul a b = rat (a.num * b.num) (a.denom * b.denom)

||| Multiply a by the multiplicative inverse of b
public export
div : Rat -> Rat -> Either String Rat
div a b = mul a !(inv b)

||| Take the rational a raised to the integral exponent b
|||
||| Negative exponents are handled correctly.
public export
pow : Rat -> Integer -> Either String Rat
pow r exp =
  if (exp < 0)
    then pow_nat !(inv r) (cast exp)
    else pow_nat r        (cast exp)
  where
    pow_nat : Rat -> Nat -> Either String Rat
    pow_nat x 0     = rat 1 1
    pow_nat x (S k) = mul x !(pow_nat x k)

export Eq Rat  where (==)    = withCrossMul (==)
export Ord Rat where compare = withCrossMul compare

public export
abs : Rat -> Either String Rat
abs (MkRat num denom zeroSafe) = rat (abs num) denom

||| Express the given rational in lowest terms
public export
simplify : Rat -> Either String Rat
simplify r = simplify_int !(abs r)
  where
    simplify_int : Rat -> Either String Rat
    simplify_int (MkRat num denom _) =
      let
        (MkDPair g _) = gcd (cast num) denom
        n = (r.num) `signedDiv` (cast g)
        d = denom `div` g
      in rat (cast n) d

||| Approximate a to the nearest 1/b
|||
||| This is surely not the most efficient method, but it'll do.
public export
approx : Rat -> Nat -> Either String Rat
approx val tolerance =
  let
    -- as an optimization, construct normalize fractional part <= 1/1
    -- so e.g. 19/12 becomes 7 / 12. this means our search is
    -- O(tolerance), rather than O(whole * tolerance)
    rem     = val.num `mod` (cast val.denom)
    whole   = val.num `div` (cast val.denom)
    proper  = rat rem val.denom
    -- now find the numerator which minimizes the error
    nearest = argmin (error !proper) 0 [1 .. (cast tolerance)]
  in
    -- don't forget to add the integral part back in
    add !(rat whole 1) !(rat nearest tolerance)
  where
    -- here we are just taking the absolute value of the difference
    -- between our guess and the input value.
    error : Rat -> Integer -> Either String Rat
    error proper n = abs $ !(sub proper !(rat n (cast tolerance)))

||| Try to coerce a rational to an integer
|||
||| This fails if the fraction can't be expressed as n/1.
public export
tryInt : Rat -> Either String Integer
tryInt r = case !(simplify r) of
  MkRat num 1 _ => Right num
  _             => Left "Not Integral"
