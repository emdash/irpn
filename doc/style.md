# Overview

I found Idris' syntax to be deceptively terse on the one hand, and
"curiously recurring" on the other. Sometimes, it feels a bit strange
and magical that Idris code does anything at all. My style evolved to
help me make sense of the language, by arranging things in ways that
either felt familiar to me, or else helped to reduce "visual clutter".

My style is still evolving. I haven't set up any style lints, because
at this stage I reserve the right to change my mind. So what follows
is *descriptive* rather than *prescriptive*, except for one major pet
peeve which I've clearly identified.

If you find yourself working on a project in Idris, keep in mind I
barely know the language. Please do *not* use this guide to justify
violating style rules in someone else's project.

What I will say is that if you're coming from mainstream languages,
you *might* find these style "hints" helpful for you as well. This is
just my opinion, I have absolutely no data to back this up. Let me
know how it goes, either way.

## Tabulating

I found that my eye has been trained on the usual `function(argument1,
argument2, ...)` syntax, and training my eye to read Idris' "curiously
recurring" patterns of syntax has taken a while.

One thing I started doing quite early is aligning code into tables.  I
call this "tabulation" or "tabulating". For example:

```
aligning_patterns (Into tables) = ...
aligning_patterns (Like   this) = ...
```

...is easier to read than:

```
not_aligning (Into tables)  = ...
not_aligning (Which creates) = ...
not_aligning (Rivers (of whitespace)) = ...
not_aligning (That run through) = ...
not_aligning (The (source code)) = ...
```

This also applies to function bodies, case expressions, let blocks,
and data definitions.

Unfortunately, I still don't fully understand what "algorithm" I'm
using in my head to do this. For the moment, it's completely
subjective and arbitrary.

A rule of thumb is to align around characters that that repeat in the
"same logical position" across lines. So, this obviously applies to
separators like `,`, `:`, `=`, and `->` -- but hold on: `(` and `)`
are often *not* a good choice, because their "logical position tends
to vary across lines (except when it doesn't).

In any case, the goal of "tabulation" is to highlight the differences
across lines by making everything that's "the same" line up and
thereby become part of the "background", leaving the things which are
different across lines to become the "foreground". Look! I'm using art
metaphors! Code is like art!

I'm don't think I'd recommend this style rule for a large institution,
unless you can reliably automate it. In fact, I probably spend too
much time tabulating code. But for now, I pretty much depend on it.

### Historical Aside

In the past, this sort of thing was frowned upon -- the main reason
being that a change to one line often triggers reformatting of the
entire table, bloating patches in ways that early code review tools
didn't handle well at all. Also, not everyone uses a proper text
editor which can make block edits.

Code review tools have come along way in the past decade -- today,
they are quite good at visually distinguishing changes to whitespace
changes from changes to program source. And emacs' `*-rectangle`
family of functions, and vim's *visual block* mode both work great for
editing code that's already tabulated.

So, I am completely comfortable with the fact that I may have to
reformat an entire table because of a change to a single "cell". It
will be perfectly clear what's happening when I review the patch, and
it's perfectly simple to modify the table in the first place with the
tools I use.

## Short definitions on one line

One thing I do like about idris is that short definitions can fit on
one or two lines:

```
imAShortFunction : String -> Bool
imAShortFunction s = length s < 10
```

If they're *really* short, I put everything on one line:

```
foo : String -> bool ; foo = length s < 10
```

And if there's a series of short definitions, then I tabulate them:

```
foo  x : String -> bool ; foo  x = length s < 10
bar  x : String -> bool ; bar  x = length s <  2
baz  x : String -> bool ; baz  x = length s <  3
quux x : String -> bool ; quux x = length s < 15
```

One downside to the above: it discourages doc comments. So I should
probably avoid this rule for exported functions. What a pity.

## Function Definitions

Whitespace is significant in Idris. I'm not a fan of this, in general
-- I tolerate it in Python -- but in Idris gets in the way of my
prefered style.

If I could, I'd format definitions with an outer `let` block like
this:

### Idris 1 Style

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c = let
  foo = munge a b
  bar = frobulate foo b
in helperFunction bar a b where
  helperFunction : Arg -> HelperReturnType
```

In fact, Idris 1 lets me do this.  Sadly, Idris 2 is a *lot* more
picky about indentation, and it won't allow `in` or `where` to be
left-justified. So, I settled for this compromise with Idris 2.

### Idris 2 Style

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c =
  let def = munge a b
  in helperFunction (munge a b)
  where
    helperFunction : Arg -> HelperReturnType
    ...
```

Or, if there are multiple let bindings, then I format like this:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c =
  let
    def1     = munge a b          -- tabulate the let bindings
    def2     = frobmugate def1 a
    flombast = def1 def2
  in
    helperFunction (flobmbasticate flombast def2)
  where
    helperFunction : Arg -> HelperReturnType
    ...
```

### The Acursed Indent: A Pet Peeve

I *strongly* dislike code formatted like this:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c = let def = munge a b            -- indent way too far!
                     in helperFunction (munge a b)  -- why?!
  where
    helperFunction : Arg -> HelperReturnType
    ....
```

Unfortunately, this is the *dominant* style for a lot of Haskell and
Idris source. And I can't stand it! It's such pointless waste of
horizontal space.

So-called "rightward drift" is as much a problem in Idris as in any
other language, and personally I see no reason to use more than two
spaces per indent level, *ever*, *in any language*, *period*.

Indent enough to make the lexical structure clear, and no more. If,
for your project or language, that's three spaces, or four -- then
fine. Whatever. But that style above? I don't care how popular it is,
I refuse to format code that way.

## Type signatures and parameter lists

When a type signature has a lot of parameters, or when the parameters
themselves have a lot of details, I place each parameter on its own
line.  If I *could* I would format like *this*.

### Idris 1 Style

```
aHungryUngryFunction
  :  TypeConstraint    a          -- note two-spaces after the colon
  => AnotherConstraint b          -- so everything is is flush
  => {an_implicit : SomeType }
  -> (a           : FirstRealArg) -- align on the ':' for named args
  -> SecondRealArg
  -> ThirdRealArg
  -> ReturnType
aHungryUngryFunction
  (Case1 a)
  b
  c
=
  ...function body
```

You can do this in Idris 1, but sadly, as with `let` above, Idris 2
requires the `=` be indented at least one level, so I settled on these
compromises:

### Idris 2: One Line Body

```
aHungryUngryFunction
  :  TypeConstraint    a
  => AnotherConstraint b
  => {an_implicit : SomeType }
  -> (a           : FirstRealArg)
  -> SecondRealArg
  -> ThirdRealArg
  -> ReturnType
aHungryUngryFunction (Case1 a) b c = oneLine a (b c)
```

The goal is simply to make it easy to skim the function signature,
while also being able to distinguish the argument list from the
function definition.

### Idris 2: Multi-Line Body

```
aHungryUngryFunction
  :  TypeConstraint    a
  => AnotherConstraint b
  => {an_implicit : SomeType }
  -> (a           : FirstRealArg)
  -> SecondRealArg
  -> ThirdRealArg
  -> ReturnType
aHungryUngryFunction (Case1 a) b c =
  case multiLine a b of
    ...
```

If the body won't fit on one line, then newline immediately after the
`=`. Otherwise, Idris' indentation rules will mandate the *acursed
indent*, described in the previous section.

### Aside: "Arrow East vs Arrow West"

What I don't like about this style is that the separator arrows
between constraints and arguments appear on the line *below* the thing
apply to:

Consider a definition like:

```
foo : Num a => Maybe a -> a
```

This means that `foo` takes an argument of type `Maybe a`, where `a`
must be an instance of `Num`, The `=>` applies to the preceeding
tokens (`Num a`).

But suppose we format like this:

```
foo
  :  Num   a
  => Maybe a
  -> a
```

Now, it's a bit less obvious that `Num a` is a type constraint, and
not an argument. You have to pay attention to whether the arrow is fat
or skinny.

Type constraints appear at the beginning of signatures, so it's not
*that* ambigious if you know the language. But one could make the case
for an "Arrow East" style:

```
aHungryUngryFunction :
  TypeConstraint    a          =>
  AnotherConstraint b          =>
  {an_implicit : SomeType }    ->
  (a           : FirstRealArg) ->
  SecondRealArg                ->
  ThirdRealArg                 ->
  ReturnType
aHungryFunction (Case1 a) b c = oneLine a (b c)
```

Or even:

```
aHungryUngryFunction :
  TypeConstraint    a =>
  AnotherConstraint b =>
  {an_implicit : SomeType } ->
  (a           : FirstRealArg) ->
  SecondRealArg ->
  ThirdRealArg ->
  ReturnType
aHungryFunction (Case1 a) b c = oneLine a (b c)
```

The thing is: I've been reading a lot of Idris source lately, and this
style seems rare to be somewhat rare. In this instance, I defer to
convention and format definitions in "arrow west".

## Data Definitions

I format data defnitions in the same spirit as function defintions,
but there's a few things to highlight:

### Plain Sum Types

#### Short Definitions on on line

```
data MyEnum = Foo | Bar | Baz
```

#### Longer Definitions on Multile Lines

Either because there are simply too many variants, or because each
variant has a long name (or many parameters), if it won't fit on one
line then each variant lives on its own line, in "pipe west"
style. Also, if variants take parameters, then they should be
tabulated.

```
data MyWordyEnum
  = MostElegentAndHonorableFoo String    String
  | MostReliableAndWorthyBar   (Maybe a) String
  | ThatScoundrelBaz           String
```

### Indexed Types

Indexed type definitions are basically always multi-line.

The key difference is that I tabulate parameters in a
"right-justified" fashion: all indexed variants must end by naming the
type under definition -- I sometimes wish that Idris had something
equivalent to Rust's `Self` keyword -- and I find it distracting if
these tokens don't visually align:

```
data CoolDependentType : Nat -> Type -> Type where
  Foo  : String    -> (a : Maybe a)         -> CoolDependentType 0     a
  Bar  : (a : Nat) -> b                     -> CoolDependentType a     b
  Baz  :              a                     -> CoolDependentType 1     a
  Quux :              CoolDependentType a b -> CoolDependentType (S a) b
```

At least for me, this makes it easier to spot that the `Quux` variant
takes one argument of a related `CoolDependentType`, and how the
indices of each variant are related to each other.

Related note: I found the syntax of indexed type definitions to be the
single-most confusing aspect of Idris and its siblings. This style is
how I managed to make sense of them.

Indexed type definitions often won't fit into 120 columns, let
alone 80. In some cases I factor out type aliases to compress the
table, but this can cause issues with type checking, and is often more
trouble than it's worth. I prefer tabulation over keeping line length
down.

Having said all this, I'm least committed to this aspect of my Idris
style, and even I find it somewhat pedantic. I would consider the
following "vertical" style acceptable, and I suspect I'll start
favoring this after a while.

```
data CoolDependentType : Nat -> Type -> Type where
  Foo
    :  String
    -> (a : Maybe a)
    -> CoolDependentType 0 a
  Bar
    :  (a : Nat)
    -> b
    -> CoolDependentType a b
  Baz
    :  a
    -> CoolDependentType 1 a
  Quux
    :  CoolDependentType a     b
    -> CoolDependentType (S a) b
```

## Record Definitions

Record definitions have the most straightforward style

```
record VeryImportandData
  constructor MkVeryImportantData
  foo  : Int
  bar  : String
  baz  : Maybe a
  quux : CoolDependentType n a
```

At a minimum, tabulate on the `:`, whose column position is dictated
by the longest field name. If multiple fields take type parameters,
I tabulate the type parameters too. Except when I don't, as above.

I don't use record types that often in Idris, as they're really more
of a special case, rather than the default.

## Interface Definitions

I don't have much to add about Interfaces. I format them like other
record and type definitions.

In some cases I prefer to collapse a bunch of related instances into a
table, as in the following actual code snippet from `Render.idr`:

```
||| Type-aware lowering to MathML elements
interface ToMathML a where
  toMathML : a -> VDom

....

ToMathML String  where toMathML s = mi () s
ToMathML VDom    where toMathML v = v
ToMathML Integer where toMathML i = mn () i
ToMathML Double  where toMathML d = mn () d
ToMathML Nat     where toMathML n = mn () n
```
