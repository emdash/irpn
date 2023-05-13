# Overview

My style evolved to help me make sense of the language, by arranging
things in ways that either felt familiar to me, or else helped to
reduce "visual clutter".

My style is still evolving. I haven't set up any style lints, because
at this stage I reserve the right to change my mind. So what follows
is *descriptive* rather than *prescriptive*, except for one major pet
peeve which I've clearly identified.

Please do *not* use this guide to justify violating style rules in
someone else's project.

If you're coming from mainstream languages, you *might* find these
style "hints" helpful. This is just my opinion, I have absolutely no
data to back this up. Let me know how it goes.

## Line Length

I am trying to keep lines under 100 characters, preferring 80. I'm not
enforcing this rule, however. I recognize that there's situations
where long lines are either necessary or more readable.

It's "best effort". I won't leave long lines in the code when I can
easily avoid it. But in a few cases I prioritize tabulation over line
length.

## Tabulation

One thing I started doing quite early is aligning code into tables,
which I find makes things easier to understand.

```
aligning_patterns (Into tables) = ...
aligning_patterns (Like   this) = ...
```

...can be easier to read than:

```
not_aligning (Into tables)  = ...
not_aligning (Which creates) = ...
not_aligning (Rivers (of whitespace)) = ...
not_aligning (That run through) = ...
not_aligning (The (source code)) = ...
```

Unfortunately, I still don't fully understand what "algorithm" I'm
using in my head to do this. What matters is how similar the text on
consecutive lines happens to be, and how many similar lines appear
consecutively.

A rule of thumb is to align around separators that that repeat in the
"same logical position" across lines, namely: `,`, `:`, `=`, and `->`.

This may bloat the line length by a few chars, or but sometimes it
results in a very wide table.

The goal of "tabulation" is to highlight the differences across lines.
By aligning text that's "the same", it becomes part of the
"background", leaving the text which varies to become the
"foreground".

I probably spend too much time tabulating code. But for now, I like
the way it looks, and I find it a useful crutch.

### Historical Aside

In the past, tabulating code was frowned upon -- the main reason being
that a change to one line often triggers reformatting of the entire
table, bloating patches in ways that early code review tools didn't
handle well at all. Also, not everyone uses a proper text editor which
can make block edits.

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

One thing I like about idris is that short definitions can fit on one
or two lines:

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

## Function Definitions with Outer Let Block

Whitespace is significant in Idris (which I'm less enamored with,
these days, but I digress). This constrains the ways in which code may
be formatted.

In Idris 1, I would format definitions with an outer `let` block like
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

Sadly, Idris 2 is a *lot* more picky about indentation, and it won't
allow `in` or `where` to be left-justified. So, I settled for these
compromises with Idris 2:

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

Or, with multiple let bindings:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c =              -- newline after `=`
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
Idris source. I just can't stand it! It's such pointless waste of
horizontal space, and it looks really ugly to me. I personally prefer,
and default to, an indent offset of *two spaces*, *period*.

I indent enough to make the nesting structure clear, and no more. I
keep it consistent within a given codebase, whether it's two spaces,
or four. I *do not* indent to align with tokens on the previous line
*just because*.

If I'm going to "waste" horizontal space, then I prefer to "waste" it
on tabulation.

The point has been made elsewhere that arguments about style are in
and of themselves a waste of time. This is is a personal project. So
I'm putting my foot down about this one thing. I will *not* use the
*accursed indent*, and that's that.

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

What I don't like about this style is that the separators appear on
the line *below* the thing apply to. Consider a definition like:

```
foo : Num a => Maybe a -> a
```

This means that `foo` takes an argument of type `Maybe a`, where `a`
must be an instance of `Num`, The `=>` applies to the preceeding
tokens (`Num a`).

Using my preferred style, we get:

```
foo
  :  Num   a
  => Maybe a
  -> a
```

Type constraints appear at the beginning of signatures, so it's not
*that* ambigious if you know the language. But one could make the case
for an "Arrow East" style:

#### Tabulated

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

#### Untabulated

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

I'm not actually sure how the community feels about this in general. I
am using "Arrow West", since that style seems to be prevalent. I'm
open to changing my mind about this.

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
"right-justified" fashion. Since all indexed variants must end by
naming the type under definition -- I sometimes wish that Idris had
something equivalent to Rust's `Self` keyword -- and I find it
distracting if these tokens don't visually align:

#### Tabulated

```
data CoolDependentType : Nat -> Type -> Type where
  Foo  : String    -> (a : Maybe a)         -> CoolDependentType 0     a
  Bar  : (a : Nat) -> b                     -> CoolDependentType a     b
  Baz  :              a                     -> CoolDependentType 1     a
  Quux :              CoolDependentType a b -> CoolDependentType (S a) b
```

Related note: I found the syntax of indexed type definitions to be the
single-most confusing aspect of Idris and its siblings. This style is
how I began to make sense of them.

When tabulated like this, such a definition often won't fit into 120
columns, let alone 80. I still prefer tabulation over keeping line
length down, in this instance.

Having said all this, I'm least committed to this aspect of my Idris
style. I find the following vertical style acceptable

#### Vertical

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
by the longest field name. If multiple fields take type parameters, I
tabulate the type parameters too. Except when I don't, as above.

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
