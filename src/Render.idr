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


||| This module implements application-specific rendering
module Render

import Data.List
import Data.String
import JS
import Web.Dom

import Common
import Input
import Calc
import State
import Layout

%default total


||| Convert a value to a string
|||
||| Works exactly like Show, except that strings aren't quoted, which
||| we would only want to do explicitly for DOM strings.
|||
||| We could also perform HTML escaping here, although I think the
||| official DOM library might do this. Does it?
interface Show t => ToString t where
  toString : t -> String
  toString = show


||| Special case for String: just return yourself.
ToString String          where toString x = x
ToString (List Char)     where toString x = pack x
ToString Char            where toString x = pack [x]
ToString (SnocList Char) where toString x = pack (asList x)
ToString Nat             where
ToString Integer         where
ToString Double          where

{- XXX:

I tried adding:

  Show a => ToString a     where

But this causes type checking to fail. It appears idris can't decide
which method to call, which is disappointing. In C++ there's a notion
that the most specific template instance "wins", while Rust does
something similar via "blanket impls".

Maybe idris supports this, I just don't know.

-}


{- Virtual Dom ************************************************************* -}


||| This is the callback passed down from main to do the actual dom
||| update.
|||
||| It's currently very specificly tied to the calculator and its types.
public export
UpdateFn : Type
UpdateFn = Calc.Action -> Types.Event -> IO ()

||| Type for attributes and event listeners
|||
||| (::=) : An ordinary attribute, a pun on (:=) which usually means assignment.
||| (::>) : An event listener, a pun on the the fat arrow in lambda syntax
public export
data Attr : Type where
  (::=) : String -> String      -> Attr
  (::>) : String -> Calc.Action -> Attr

infixl 10 ::=
infixl 10 ::>

||| This interface abstracts over different ways of specifying properties
interface Attrs a where
  bind : UpdateFn -> Element -> a -> JSIO ()

||| You may not have any properties to bind.
Attrs () where
  bind _ _ _ = pure ()

||| You may bind a single property or event handler
Attrs Render.Attr where
  bind update element (key ::= value) = do
    ignore $ setAttribute element key (toString value)
  bind update element (event ::> action) = do
    listener <- toEventListener (update action)
    ignore $ addEventListener element event (Just listener)

||| You may bind a list of attributes and event handlers
Attrs a => Attrs (List a) where
  bind update element attrs = do
    for_ attrs $ \attr => do
      bind update element attr

||| You may bind a maybe of Attr
Attrs a => Attrs (Maybe a) where
  bind update element Nothing  = do pure ()
  bind update element (Just a) = do bind update element a

||| You may bind pairs and tuples of Attrs
Attrs a => Attrs b => Attrs (a, b) where
  bind update element (x, y) = do
    bind update element x
    bind update element y


||| An interface which child dom nodes must implement.
interface Child a where
  attach : UpdateFn -> Element -> a -> JSIO ()

||| A virtual dom subtree
|||
||| I introduced this to keep the rendering layer pure, pushing the
||| JSIO monad to the top-level
|||
||| Variants:
||| - T:  Text Nodes
||| - E:  Standard elements
||| - NS: Elements with an explicit namespace
public export
data VDom : Type where
  T      : ToString v =>                                     v -> VDom
  E      : Attrs a    => Child c =>           String -> a -> c -> VDom
  NS     : Attrs a    => Child c => String -> String -> a -> c -> VDom

mutual
  ||| Construct real DOM tree from a VDom tree, in the JSIO monad.
  |||
  |||   Thanks to Stephan Hoek for helping in the early stages of this.
  |||
  ||| What's crucial here is that all operations return the parent,
  ||| otherwise the resulting tree would have the wrong shape.
  |||
  ||| W/R/T event listeners: we translate the dom events to high-level
  ||| calculator actions here, using the update function given.
  public export partial
  vrender : UpdateFn -> VDom -> JSIO Node
  vrender update (T value) = do
    ret <- createTextNode !document (toString value)
    pure $ ret :> Node
  vrender update (E tag attrs contents) = do
    ret <- createElement !document tag
    bind   update ret attrs
    attach update ret contents
    pure $ ret :> Node
  vrender update (NS ns tag attrs contents) = do
    ret <- createElementNS !document (Just ns) tag
    bind   update ret attrs
    attach update ret contents
    pure $ ret :> Node

  ||| You may append nothing at all.
  Child () where
    attach _ _ _ = pure ()

  ||| You may append any ToString type.
  ToString a => Child a where
    attach _ parent x = do
      text <- createTextNode !document (toString x)
      ignore $ parent `appendChild` text

  ||| You may append an arbitrary VDom subtree.
  Child VDom where
    attach update parent vchild = do
      child <- vrender update vchild
      ignore $ parent `appendChild` child

  ||| You may append pairs and tuples of any otherwise appendable type.
  Child a => Child b => Child (a, b) where
    attach update parent (x, y) = do
      attach update parent x
      attach update parent y

  ||| You may append a list of any otherwise appendable type.
  Child a => Child (List a) where
    attach update parent children = do
      for_ children $ \child => do
        attach update parent child

  ||| You may append a maybe of any otherwise appendable type.
  |||
  ||| Appending Nothing is allowed, and is a no-op.
  Child a => Child (Maybe a) where
    attach update parent Nothing      = pure ()
    attach update parent (Just child) = attach update parent child

  ||| You may append an either of any two otherwise appendable types.
  |||
  ||| This will always insert an element into the dom.
  Child l => Child r => Child (Either l r) where
    attach update parent (Left  child) = attach update parent child
    attach update parent (Right child) = attach update parent child

  ||| You may append an either where only the left child is appendable.
  Child l => Child (Either l b) where
    attach update parent (Left child) = attach update parent child
    attach _      _      _            = pure ()

  ||| You may append an either where only the right child is appendable.
  Child r => Child (Either a r) where
    attach update parent (Right child) = attach update parent child
    attach _      _      _             = pure ()

{- Quick-and-Dirty DSL for the subset of HTML I use -}

div    : Attrs a => Child c => a -> c -> VDom ; div    = E "div"
span   : Attrs a => Child c => a -> c -> VDom ; span   = E "span"
h1     : Attrs a => Child c => a -> c -> VDom ; h1     = E "h1"
button : Attrs a => Child c => a -> c -> VDom ; button = E "button"
li     : Attrs a => Child c => a -> c -> VDom ; li     = E "li"
ul     : Attrs a => Child c => a -> c -> VDom ; ul     = E "ul"
tr     : Attrs a => Child c => a -> c -> VDom ; tr     = E "tr"
td     : Attrs a => Child c => a -> c -> VDom ; td     = E "td"
table  : Attrs a => Child c => a -> c -> VDom ; table  = E "table"

{- Quick-and-Dirty DSL for the subset of MathML I use -}

||| Create an element in the MathML namespace
mathml : Attrs a => Child c => String -> a -> c -> VDom
mathml = NS "http://www.w3.org/1998/Math/MathML"

math  : Attrs a => Child c => a -> c -> VDom ; math  = mathml "math"
mfrac : Attrs a => Child c => a -> c -> VDom ; mfrac = mathml "mfrac"
mrow  : Attrs a => Child c => a -> c -> VDom ; mrow  = mathml "mrow"
mi    : Attrs a => Child c => a -> c -> VDom ; mi    = mathml "mi"
mn    : Attrs a => Child c => a -> c -> VDom ; mn    = mathml "mn"

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
fraction num denom = (mfrac () [toMathML num, toMathML denom])

||| Render a mixed number from its parts
mixed
  :  ToMathML i
  => ToMathML n
  => ToMathML d
  => i
  -> n
  -> d
  -> VDom
mixed int num denom = mrow () [toMathML int, fraction num denom]

-- the main thing to note here is that strings and dom elements are
-- rendered as `mi` while numeric types are rendered as `mn`.
ToMathML String  where toMathML s = mi () s
ToMathML VDom    where toMathML v = v
ToMathML Integer where toMathML i = mn () i
ToMathML Double  where toMathML d = mn () d
ToMathML Nat     where toMathML n = mn () n

||| Renders a Rat as a mixed number or proper fraction.
ToMathML Rat where
  toMathML (MkRat num denom) =
    if (abs num) < (cast denom)
    then fraction num denom
    else let
      whole := (num `div` (cast denom))
      num   := (num `mod` (cast denom))
    in mixed whole num denom

||| Now implement for arbitrary stack values
ToMathML Common.Value where
  toMathML (I i)   = toMathML i
  toMathML (F dbl) = toMathML dbl
  toMathML (R x)   = toMathML x
  toMathML (S str) = toMathML str
  toMathML (P sx)  = mi () "Not Implemented"

{- Other Helper Functions ************************************************** -}

||| Renders a labeled container
container : Child c => String -> String -> c -> VDom
container id name contents =
  div ["id" ::= id, "class" ::= "grid"] (h1 () name, contents)

||| Creates a small list of mutually-exclusive choices.
|||
||| The selected choice is given a special attribute
radioGroup : String -> List (String, VDom, Action) -> List VDom
radioGroup _                          [] = []
radioGroup selected ((key, label, action) :: xs) = let
  attrs = if selected == key
    then ["click" ::> action, "selected" ::= "true"]
    else ["click" ::> action]
  in (button attrs label) :: (radioGroup selected xs)

||| Table of unicode symbols for operators that have an obvious choice
|||
||| For some of these, we can return unicode strings which will work
||| anywhere. In the particular case of fractions, MathML looks a lot
||| better - with the caveat that it only works in Firefox without a
||| polyfill.
symbols : String -> Maybe VDom
symbols "exch"   = Just (T "\u{2B0D}")
symbols "add"    = Just (T "+")
symbols "sub"    = Just (T "-")
symbols "mul"    = Just (T "⨉")
symbols "div"    = Just (T "÷")
symbols "pow"    = Just (T "x\u{207F}")
symbols "exp"    = Just (T "\u{1D486}\u{207F}")
symbols "square" = Just (T "x\u{00B2}")
symbols "abs"    = Just (T "|\u{1D499}|")
symbols "sqrt"   = Just (T "\u{221A}")
symbols "E"      = Just (T "\u{1D486}")
symbols "PI"     = Just (T "\u{1D70B}")
symbols "frac"   = Just (T "fraction")
symbols "fadd"   = Just (T "+")
symbols "fsub"   = Just (T "-")
symbols "fmul"   = Just (T "⨉")
symbols "fdiv"   = Just (T "÷")
symbols "f2"     = Just (fraction "x"   2)
symbols "f4"     = Just (fraction "x"   4)
symbols "f8"     = Just (fraction "x"   8)
symbols "f16"    = Just (fraction "x"  16)
symbols "finv"   = Just (fraction 1   "x")
symbols _        = Nothing

||| Render the accumulator's blinky cursor
|||
||| Given a string-representable value, it will return a VDom element
||| where the last character has been wrapped in an element with the
||| carret ID, so that special CSS rules can match on it.
carret : ToString a => Maybe a -> VDom
carret Nothing  = span ("id" ::= "carret") ()
carret (Just v) =
  let
    s     = toString v
    len   = cast (String.length s)
    end   = len - 1
    head  = strSubstr 0 end s
    last  = strSubstr end len s
  in mn () (head, span ("id" ::= "carret") (), last)

||| Render the accumulator contents
contents : Accum -> VDom
contents Empty         = carret (the (Maybe Nat) Nothing)
contents (Digits i)    = carret (Just i)
contents (Decimal i j) = carret (Just (toString i ++ "." ++ toString j))
contents (Num i j)     = math () (mixed i (carret j) "?")
contents (Denom i j k) = math () (mixed i j (carret k))
contents (Id var)      = carret (Just var)

||| Render the accumulator
|||
||| If error is non-empty, then set the err attribute on the resulting
||| element.
public export
render_accum : Accum -> Maybe String -> VDom
render_accum accum err = div
  ("id" ::= "accum", map ("err" ::=) err)
  (contents accum)

{- In the above:

  map ("err" ::=) err : Maybe String -> Maybe Attr

  ("err" ::=) : String -> Attr

  This seems like a useful idiom for binding properties to maybe
  values. I could imagine a (::?) operator which handles this.
-}


{-
  key : Accum -> String -> Key -> JSIO Element
  key accum label k = do
    ret <- createElement  !document "button"
    txt <- createTextNode !document label
    ignore $ appendChild      ret txt
    on ret "click" (onclick accum "foo" k)
    pure ret
  where
    onclick : Accum -> String -> Key -> Types.Event -> IO ()
    onclick accum msg k _ = do
      consoleLog msg
      update (enterKey k accum)

-}

public export
tools : VDom
tools = div
  ("id" ::= "tools")
  (
    button ("click" ::> Key Clear) "Clear",
    button ("click" ::> Reset)     "Reset",
    button ("click" ::> Undo )     "Undo",
    button ("click" ::> Redo )     "Redo"
  )

public export
stack : List Common.Value -> VDom
stack vs = container "stack-container" "Stack" (map toMathML vs)

public export
vars : VDom
vars = container "vars-container" "Vars" "not implemented"

public export
tape : VDom
tape = container "tape-container" "Tape" "not implemented"

public export
keypad : VDom
keypad = div ("id" ::= "content") ()

public export
content : Calc -> VDom
content calc =
  if calc.showing == "keyboard"
  then tape
  else keypad

public export
mode : Calc -> VDom
mode calc = div
  ("id" ::= "mode")
  (radioGroup calc.showing (map item layouts))
  where
    item : Layout -> (String, VDom, Action)
    item lyt = let n = name lyt in (n, T n, Show n)

||| Render the entire calculator
|||
||| XXX: this is work in progress
public export
render_calc : Calc -> Maybe String -> VDom
render_calc calc err =
  div
    ("id" ::= "state", "showing" ::= calc.showing)
    [
      mode calc,
      tools,
      stack calc.state.stack,
      content calc,
      render_accum calc.state.accum err
    ]
