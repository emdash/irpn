module Render

import Data.String
import JS
import Web.Dom

import Common
import Input
import Calc


%default total


||| Like Show, except strings and chars are presented as-is.
interface Show t => ToString t where
  toString : t -> String
  toString = show

ToString String where
  toString x = x

ToString Char where
  toString x = pack [x]

ToString (List Char) where
  toString x = pack x

ToString Nat     where
ToString Integer where
ToString Double  where


||| Quick-and-Dirty Virtual Dom
|||
||| I introduced this to keep the rendering code pure, to avoid having
||| to construct everything within the JSIO monad.
|||
||| It's possible that I could avoid this abstraction layer, and
||| re-factor the functions in here entirely in terms of `JSIO Node`,
||| but I'm going to keep it for now.
|||
||| : (++)  => `appendChild`
||| : (+*)  => `appendChild [....]`
||| : (<:)  => `setAttribute`
||| : (<*)  => `addEventListener` (TBD)
export
data VDom : Type where
  T    :           String           -> VDom
  E    :           String           -> VDom
  NS   : String -> String           -> VDom
  (<:) : VDom   -> (String, String) -> VDom
  (++) : VDom   -> VDom             -> VDom
  (+*) : VDom   -> List VDom        -> VDom

infixl 10 <:
infixl 10 +*

-- Quick-and-Dirty DSL for the html subset I use in this project
-- not going to document each function here, as this is all just boilerplate
t      : ToString a => a -> VDom
t x  = T (toString x)
div    : VDom             ; div    = E "div"
span   : VDom             ; span   = E "span"
h1     : VDom             ; h1     = E "h1"
button : VDom             ; button = E "button"
li     : VDom             ; li     = E "li"
ul     : VDom             ; ul     = E "ul"
tr     : VDom             ; tr     = E "tr"
td     : VDom             ; td     = E "td"
table  : VDom             ; table  = E "table"

||| Construct real DOM tree from a VDom tree, in the JSIO monad.
|||
||| XXX: the return type should be Node, but I'm too dumb to figure
||| out how to handle this. Thanks to Stephan Hoek for getting me this
||| far.
public export partial
vrender : VDom -> JSIO Element
-- case for text nodes
vrender (T str) = do
  -- XXX: this is wrong, but the types don't line up unless I wrap the
  -- inner text in a span.
  --
  -- Things probably won't look right with the extra <span> elements,
  -- but this lets me move on for now.
  ret  <- createElement' !document "span"
  text <- createTextNode !document str
  ignore $ ret `appendChild` text
  pure ret
-- case for element nodes
vrender (E tag) = do
  ret <- createElement'   !document tag
  pure ret
-- case for namespaced element nodes
vrender (NS ns tag) = do
  ret <- createElementNS' !document (Just ns) tag
  pure ret
-- case for setting an attribute
vrender (x <: (k, v)) = do
  ret <- vrender x
  setAttribute ret k v
  pure ret
-- case for appending a child element
vrender (x ++ y) = do
  parent <- vrender x
  child  <- vrender y
  ignore $ parent `appendChild` child
  pure parent
-- case for spreading append of a list of child elements
vrender (x +* ys) = do
  parent <- vrender x
  for_ ys $ \c => do
    child <- vrender c
    ignore $ parent `appendChild` child
  pure parent

||| Renders a labeled container
container : String -> VDom -> VDom
container id name = div
  <: ("id",        id)
  <: ("class", "grid")
  ++ (h1 ++ name)

||| Constructs a group of items representing a mutually-exclusive choice
|||
||| XXX: it'd be interesting to be able to reflect on an ADT and
||| derive a radioGroup from the constructors, for now just do a more
||| literal translation.
|||
||| XXX: action is missing
radioGroup : String -> List (String, VDom) -> List VDom
radioGroup _                          [] = []
radioGroup selected ((key, label) :: xs) =
  let ret = button ++ label
  in if key == selected
    then (ret <: ("selected", "true")) :: radioGroup selected xs
    else ret :: radioGroup selected xs


-- Quick-and-Dirty dsl for the subset of MathML I use in this project

||| Create an element in the MathML namespace
mathml : String -> VDom

-- not going to document this boilerplate
mathml = NS "http://www.w3.org/1998/Math/MathML"
math  : VDom  ; math  = mathml "math"
mfrac : VDom  ; mfrac = mathml "mfrac"
mrow  : VDom  ; mrow  = mathml "mrow"
mi    : VDom  ; mi    = mathml "mi"
mn    : VDom  ; mn    = mathml "mn"

{- define a toMathML function for some likely types -}
interface ToMathML a where
  toMathML : a -> VDom

ToMathML String where
  toMathML s = mi ++ t s

ToMathML VDom where
  toMathML v = mi ++ v

ToMathML Integer where
  toMathML 0 = mn
  toMathML i = mn ++ t i

ToMathML Nat where
  toMathML 0 = mn
  toMathML n = mn ++ t n

ToMathML a => ToMathML (Maybe a) where
  toMathML Nothing  = t ""
  toMathML (Just x) = toMathML x

||| Render a leaf fraction from its parts
fraction : ToMathML a => ToMathML b => a -> b -> VDom
fraction num denom = mfrac ++ toMathML num ++ toMathML denom

||| Render a leaf mixed number from its parts
mixed
  :  ToMathML i
  => ToMathML n
  => ToMathML d
  => i
  -> n
  -> d
  -> VDom
mixed int num denom = mrow ++ (toMathML int) ++ (fraction num denom)

ToMathML Rat where
  toMathML (MkRat num denom) =
    if (abs num) < (cast denom)
    then fraction                       num                      denom
    else mixed (num `div` (cast denom)) (num `mod` (cast denom)) denom

||| Table of unicode symbols for operators that have an obvious choice
|||
||| For some of these, we can return unicode strings which will work
||| anywhere. In the particular case of fractions, MathML looks a lot
||| better - with the caveat that it only works in Firefox without a
||| polyfill.
symbols : String -> Maybe VDom
symbols "exch"   = Just (t                        "\u{2B0D}")
symbols "add"    = Just (t                               "+")
symbols "sub"    = Just (t                               "-")
symbols "mul"    = Just (t                               "⨉")
symbols "div"    = Just (t                               "÷")
symbols "pow"    = Just (t                       "x\u{207F}")
symbols "exp"    = Just (t               "\u{1D486}\u{207F}")
symbols "square" = Just (t                       "x\u{00B2}")
symbols "abs"    = Just (t                     "|\u{1D499}|")
symbols "sqrt"   = Just (t                        "\u{221A}")
symbols "E"      = Just (t                       "\u{1D486}")
symbols "PI"     = Just (t                       "\u{1D70B}")
symbols "frac"   = Just (t                        "fraction")
symbols "fadd"   = Just (t                               "+")
symbols "fsub"   = Just (t                               "-")
symbols "fmul"   = Just (t                               "⨉")
symbols "fdiv"   = Just (t                               "÷")
symbols "f2"     = Just (fraction "x"                      2)
symbols "f4"     = Just (fraction "x"                      4)
symbols "f8"     = Just (fraction "x"                      8)
symbols "f16"    = Just (fraction "x"                     16)
symbols "finv"   = Just (fraction   1                    "x")
symbols _        = Nothing

||| Return a representation of the given value
display : Common.Value -> VDom
display (I   i) = div ++ (t (show i))
display (F dbl) = div ++ (t (show dbl))
display (R rat) = div ++ (toMathML rat)
display (S str) = case symbols str of
  Nothing => t str
  Just d  => d
display (P sx)  = t "not implemented"

||| Render the accumulator's carret / cursor
carret : ToString a => Maybe a -> VDom
carret Nothing  = mi ++ (span <: ("id", "carret"))
carret (Just v) =
  let
    s     = toString v
    len   = cast (String.length s)
    end   = len - 1
    head  = strSubstr 0 end s
    last  = strSubstr end len s
  in
    mi ++ (t head) ++ (span <: ("id", "carret") ++ (t last))

||| Render the accumulator contents
contents : Accum -> VDom
contents Empty         = carret (the (Maybe Nat) Nothing)
contents (Digits i)    = carret (Just i)
contents (Decimal i j) = span ++ (t i) ++ (t ".") ++ (carret (Just j))
contents (Num i j)     = mixed i (carret j) "?"
contents (Denom i j k) = mixed i j (carret k)
contents (Id var)      = carret (Just (pack (asList var)))

||| Render the accumulator
public export
render_accum : Accum -> Maybe String -> VDom
render_accum accum Nothing    = div <: ("id", "accum")                 ++ contents accum
render_accum accum (Just err) = div <: ("id", "accum") <: ("err", err) ++ contents accum
