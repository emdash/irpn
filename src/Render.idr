module Render

import Data.String
import JS
import Web.Dom

import Common
import Input
import Calc


%default total


||| Like Show, except it doesn't quote strings and characters, as it's
||| meant for DOM strings rather than repl/CLI useage.
interface Show t => ToString t where
  toString : t -> String
  toString = show
ToString String          where toString x = x
ToString Char            where toString x = pack [x]
ToString (List Char)     where toString x = pack x
ToString (SnocList Char) where toString x = pack (asList x)
ToString Nat             where
ToString Integer         where
ToString Double          where

{- Virtual Dom ************************************************************* -}

||| Quick-and-Dirty Virtual Dom
|||
||| I introduced this to keep the rendering code pure, to avoid having
||| to construct everything within the JSIO monad.
|||
||| This data-type represents *operations* on dom elements, so it is
||| inverted relative to the DOM. `vrender` builds the resulting DOM
||| from left to right.
|||
||| I may re-factor the code in this module to remove this VDom
||| layer. Or I could expand it into a proper VDom that does diffing,
||| or adopt one of the VDom libraries that already exist. I haven't decided.
|||
||| : (++)  => append node on right to parent on left
||| : (+*)  => append list of nodes on right to parent on left
||| : (+:)  => append string on right to parent on left as a text node
||| : (<:)  => set the attribute on the right on the node on left
||| : (<*)  => (TBD) bind the given event handler on right to node on left
export
data VDom : Type where
  E    :                         String           -> VDom
  NS   :               String -> String           -> VDom
  (<:) :               VDom   -> (String, String) -> VDom
  (+:) : ToString a => VDom   -> a                -> VDom
  (++) :               VDom   -> VDom             -> VDom
  (+*) :               VDom   -> List VDom        -> VDom

-- XXX: are we sure the fixity and priority are right here?
infixl 10 <:
infixl 10 +:
infixl 10 +*

div    : VDom ; div    = E "div"
span   : VDom ; span   = E "span"
h1     : VDom ; h1     = E "h1"
button : VDom ; button = E "button"
li     : VDom ; li     = E "li"
ul     : VDom ; ul     = E "ul"
tr     : VDom ; tr     = E "tr"
td     : VDom ; td     = E "td"
table  : VDom ; table  = E "table"

||| Construct real DOM tree from a VDom tree, in the JSIO monad.
|||
|||   Thanks to Stephan Hoek for helping in the early stages of this.
|||
||| What's crucial here is that all the binary operations return the
||| left hand side as the result, otherwise the resulting tree would
||| have the wrong shape.
public export partial
vrender : VDom -> JSIO Element
vrender (E tag) = do
  ret <- createElement'   !document tag
  pure ret
vrender (NS ns tag) = do
  ret <- createElementNS' !document (Just ns) tag
  pure ret
vrender (x <: (k, v)) = do
  ret <- vrender x
  setAttribute ret k v
  pure ret
vrender (x +: str) = do
  parent <- vrender x
  text <- createTextNode !document (toString str)
  ignore $ parent `appendChild` text
  pure parent
vrender (x ++ y) = do
  parent <- vrender x
  child  <- vrender y
  ignore $ parent `appendChild` child
  pure parent
vrender (x +* ys) = do
  parent <- vrender x
  for_ ys $ \c => do
    child <- vrender c
    ignore $ parent `appendChild` child
  pure parent


{- Quick-and-Dirty DSL for the subset of MathML I use in this project ****** -}


||| Create an element in the MathML namespace
mathml : String -> VDom
mathml = NS "http://www.w3.org/1998/Math/MathML"

math  : VDom  ; math  = mathml "math"
mfrac : VDom  ; mfrac = mathml "mfrac"
mrow  : VDom  ; mrow  = mathml "mrow"
mi    : VDom  ; mi    = mathml "mi"
mn    : VDom  ; mn    = mathml "mn"

||| Type-aware lowering to MathML elements
interface ToMathML a where
  toMathML : a -> VDom

||| Render a fraction from its parts
fraction
  :  ToMathML a
  => ToMathML b
  => a
  -> b
  -> VDom
fraction num denom = (mfrac ++ toMathML num) ++ toMathML denom

||| Render a mixed number from its parts
mixed
  :  ToMathML i
  => ToMathML n
  => ToMathML d
  => i
  -> n
  -> d
  -> VDom
mixed int num denom = (mrow ++ toMathML int) ++ fraction num denom

-- the main thing to note here is that strings and dom elements are
-- rendered as `mi` while numeric types are rendered as `mn`.
ToMathML String  where toMathML s = mi +: s
ToMathML VDom    where toMathML v = v
ToMathML Integer where toMathML i = mn +: i
ToMathML Double  where toMathML d = mn +: d
ToMathML Nat     where toMathML n = mn +: n

||| Renders a Rat as a mixed number or proper fraction.
ToMathML Rat where
  toMathML (MkRat num denom) =
    if (abs num) < (cast denom)
    then fraction num denom
    else
      let
        whole := (num `div` (cast denom))
        num   := (num `mod` (cast denom))
      in
        mixed whole num denom


{- Other Helper Functions ************************************************** -}


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


||| Table of unicode symbols for operators that have an obvious choice
|||
||| For some of these, we can return unicode strings which will work
||| anywhere. In the particular case of fractions, MathML looks a lot
||| better - with the caveat that it only works in Firefox without a
||| polyfill.
symbols : String -> Maybe VDom
symbols "exch"   = Just (mi +:            "\u{2B0D}")
symbols "add"    = Just (mi +:                   "+")
symbols "sub"    = Just (mi +:                   "-")
symbols "mul"    = Just (mi +:                   "⨉")
symbols "div"    = Just (mi +:                   "÷")
symbols "pow"    = Just (mi +:           "x\u{207F}")
symbols "exp"    = Just (mi +:   "\u{1D486}\u{207F}")
symbols "square" = Just (mi +:           "x\u{00B2}")
symbols "abs"    = Just (mi +:         "|\u{1D499}|")
symbols "sqrt"   = Just (mi +:            "\u{221A}")
symbols "E"      = Just (mi +:           "\u{1D486}")
symbols "PI"     = Just (mi +:           "\u{1D70B}")
symbols "frac"   = Just (mi +:            "fraction")
symbols "fadd"   = Just (mi +:                   "+")
symbols "fsub"   = Just (mi +:                   "-")
symbols "fmul"   = Just (mi +:                   "⨉")
symbols "fdiv"   = Just (mi +:                   "÷")
symbols "f2"     = Just (fraction "x"   2)
symbols "f4"     = Just (fraction "x"   4)
symbols "f8"     = Just (fraction "x"   8)
symbols "f16"    = Just (fraction "x"  16)
symbols "finv"   = Just (fraction 1   "x")
symbols _        = Nothing

||| Return a representation of the given value
display : Common.Value -> VDom
display (I   i) = div +: i
display (F dbl) = div +: dbl
display (R rat) = math ++ (toMathML rat)
display (S str) = case symbols str of
  Nothing => div +: str
  Just d  => d
display (P sx)  = span +: "not implemented"

||| Render the accumulator's carret / cursor
|||
||| Given a string-representable value, it will return a VDom element
||| where the last character has been wrapped in the carret element,
||| so that CSS rules can match on it.
carret : ToString a => Maybe a -> VDom
carret Nothing  = span <: ("id", "carret")
carret (Just v) =
  let
    s     = toString v
    len   = cast (String.length s)
    end   = len - 1
    head  = strSubstr 0 end s
    last  = strSubstr end len s
  in
    mn +: head ++ (span <: ("id", "carret") +: last)

||| Render the accumulator contents
contents : Accum -> VDom
contents Empty         = carret (the (Maybe Nat) Nothing)
contents (Digits i)    = carret (Just i)
contents (Decimal i j) = carret (Just (toString i ++ "." ++ toString j))
contents (Num i j)     = math ++ mixed i (carret j) "?"
contents (Denom i j k) = math ++ mixed i j          (carret k)
contents (Id var)      = carret (Just var)

||| Render the accumulator
|||
||| If error is non-empty, then set the err attribute on the resulting
||| element.
public export
render_accum : Accum -> Maybe String -> VDom
render_accum accum err =
  let
    ret = div <: ("id", "accum")
  in case err of
    Nothing    => ret
    Just err   => ret <: ("err", err)
  ++ contents accum


||| Some helper functions for testing tree rendering in the repl.
namespace Test

  ||| Emulates the shape of the browser DOM, i.e. not an inverted tree
  public export
  data TestDom
    = Element (Maybe String) String (SnocList (String, String)) (SnocList TestDom)
    | Text String

  ||| Append child node to existing element on LHS
  partial
  append : TestDom -> TestDom -> TestDom
  append (Element ns tag attrs children) child = 
          Element ns tag attrs (children :< child)

  partial
  setAttr : TestDom -> (String, String) -> TestDom
  setAttr (Element ns tag attrs           children) attr = 
           Element ns tag (attrs :< attr) children

  public export partial
  trender : VDom -> TestDom
  trender (E tag)     = (Element Nothing   tag Lin Lin)
  trender (NS ns tag) = (Element (Just ns) tag Lin Lin)
  trender (x <: y)    = (trender x) `setAttr` y
  trender (x +: y)    = (trender x) `append` (Text (toString y))
  trender (x ++ y)    = (trender x) `append` (trender y)
  trender (x +* xs)   = foldl (append) (trender x) (map trender xs)

  public export partial
  pprint : TestDom -> Nat -> IO ()
  pprint (Element _ tag attrs children) depth = do
    let attrs = joinBy " " (map (\(k, v) => k ++ ": " ++ v) (asList attrs))
    putStrLn ((indent depth " ") ++ tag ++ " " ++ attrs)
    for_ children $ \child => do
      pprint child (depth + 1)
  pprint (Text t) depth = do
    putStrLn ((indent depth " ") ++ (show t))

  public export partial
  taccum : SnocList Key -> IO ()
  taccum keys = case Input.test keys of
    Nothing    => putStrLn "empty"
    Just accum => pprint (trender (render_accum accum Nothing)) 0
