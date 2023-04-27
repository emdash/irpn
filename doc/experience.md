# Overview

I struggled even with Idris, at first. I started working with Idris in
October 2022. It's April 2023 at the time of this writing, and I've
*finally* begun to understand Idris.

Life has been happening in the meantime -- but having the patience to
work through [the
book](https://www.manning.com/books/type-driven-development-with-idris)
paid off, and now I finally know enough to be write an Idris port of
code originally written in JavaScript.

## First Step: Read [The Book](https://www.manning.com/books/type-driven-development-with-idris)

Having only casual experience with Haskell and related languages, I
found that working through [The
book](https://www.manning.com/books/type-driven-development-with-idris)
was essential.

It's accessible at the expense of being repetitious. I wished that
later chapters focused mainly on the *changes* to the program source,
rather than spelling out each step in detail. However, whenever I
would skim ahead, I'd miss crucial details and make mistakes. On the
other hand, some of those mistakes provided valuable experience in
interpeting diagnostic messages from the type checker.

In all, the biggest gripe I have is that the book still assumes
Idris 1. And while there is a [list of changes](TDD) available online,
I didn't want to add any more to my already taxed cognitive load. I do
think it's about time an updated edition of the book was published.

In the mean time, there is [stephan hoeck's tutorial](TBD) which is a
promising introduction to Idris2. I started working through it, but
there was too much overlap with what I'd already covered. I plan to
revisit it when I need a break from writing cod.e

## Bugs, Gotchas, and Advice

What follows is a list of issues and nits that make Idris2 not quite
production quality. I've spent a lot of time on the fringes of
computing, so nothing in here is a deal-breaker for me,
personally. But consider how much these would affect you before diving
in.

- Idris2 seems more stable than Idris1, I still manage to wedge my
  editor session on a regular basis. If things start acting weird, try
  restarting your editor before you do anything drastic.
- It's possible to write code that type-checks, but can't be executed.
  - So run your code often!
- It's possible to define types that you cannot implement.
  - write your types in parallel with some representative uses, to
    spot this early.
- in the emacs mode, I have to type-check twice the first time I load
  a file.
  - the first time, it can't seem to find the file.

## Practical Advice

- Do all the exercises in the first few chapters of [the
  book](https://www.manning.com/books/type-driven-development-with-idris)!
  - Type everything in yourself!
  - Practice the key-strokes, as they are key to being productive in idris.
- Compiler error messages can be inscrutable and misleading.
  - If an error doesn't make sense, it might be a compiler issue.
- You can comment out to the end of the file with `{-`,
  - which is handy for figuring out where the error *really* is.
- Check into source control often.
- Compile your whole project often
- Write your `.ipkg` the minute your project goes beyond a single file.
- Use [idris2-pack](TBD) the minute you decide you need an external dependency
- Test and run your code in the REPL often.

## Emacs Cheat Sheet

This wasn't in the book or the official documentation, but a bit of
googleing found me this:

## Stuff that's Not in the Book

- using pack

## A word about my coding style

I found Idris' syntax to be deceptively terse. It can sometimes seem a
bit strange that a bit of code does anything at all.

I found that my eye has been trained on the usual `function(argument1,
argument2, ...)` syntax, and learning to visually parse Idris's
`function argument1 argument 2` syntax was difficult. So one thing I
tend to do is...

```
align_patterns (Into        tables)          = ...
align_patterns (Like        this)            = ...
align_patterns (So          (that you)  can) = ...
align_patterns (See         what)            =.
align_patterns Matters                       = ...
align_patterns AtAGlance                     = ...
```

I find this is easier to read than:

```
not_aligning (Into tables)  = ...
not_aligning (Which creates) = ...
not_aligning (Rivers (of whitespace)) = ...
not_aligning (That run through) = ...
not_aligning (The (source code)) = ...
not_aligning (In ways that obscure) = ...
not_aligning (What (is happening)) = ..
```


Though as time goes on, I need this visual aid less, I find It's
easier to edit, so long as your text editor emacs supports rectangular
edits. Vim, emacs, helix, and kakoune all do this.

The default idris style is not something I particularly care for, and
whitespace is significant in Idris. I'm not a fan of this, in general.

If I could, I'd format definitions with an outer `let` block like
this:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c = let
  def = munge a b
in helperFunction (munge a b) where
  helperFunction : Arg -> HelperReturnType
```

Sadly, Idris2 is a *lot* more picky about indentation than Idris1. So
this is how I have to format it in Idris2:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c =
  let def = munge a b
  in helperFunction (munge a b)
  where
	helperFunction : Arg -> HelperReturnType
	...
```

If the outer let block has multiple definitions, I format like this:

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c =
  let
	def1     = munge a b
	def2     = frobmugate def1 a
	flombast = def1 def2
  in
	helperFunction (flobmbasticate flombast def2)
  where
	helperFunction : Arg -> HelperReturnType
	...
```

I strongly dislike code formatted like this. Unfortunately, this is
what emacs wants to do if left to its own devices.

```
someFunction : Takes -> Some -> Args -> ReturnType
someFunction a b c = let
                     def = munge a b
					 in helperFunction (munge a b)
  where
	helperFunction : Arg -> HelperReturnType
	....
```

When a type signature has a lot of parameters, or when the parameters
themselves have a lot of details, I break them up into multiple
lines. If I *could* I would do it like *this*.

```
aHungryUngryFunction
	:  TypeConstraint    a  -- note two-spaces after the colon
	=> AnotherConstraint b  -- thta keeps everything flush
	=> {an_implicit : SomeType }
	-> (a : FirstRealArg)
	-> SecondRealArg
	-> ThirdRealArg
	-> FourArg
	-> ReturnType
aHungryFunction
	(Case1 a)
	b
	c
	d
=
	...function body
```
Sadly, idris requires the `=` be indented at least one level, so you
have to write it like this:

```
aHungryUngryFunction
	:  TypeConstraint    a  -- note two-spaces after the colon
	=> AnotherConstraint b  -- thta keeps everything flush
	=> {an_implicit : SomeType }
	-> (a : FirstRealArg)
	-> SecondRealArg
	-> ThirdRealArg
	-> FourArg
	-> ReturnType
aHungryFunction
	(Case1 a)
	b
	c
	d
	= ...function body
```


If a function body fits on one line, I leave it that way.

```
imAShortFunction : String -> Bool
imAShortFunction s = length s < 10
```

If they're *really* short, I put everything on one line

```
foo : String -> bool ; foo = length s < 10
```

If a series of similar definitions are consecutive, these also become
a table, to avoid the "rivers of whitespace" and help highlight what
actually matters.

```
foo  x : String -> bool ; foo  x = length s < 10
bar  x : String -> bool ; bar  x = length s < 2
baz  x : String -> bool ; baz  x = length s < 3
quux x : String -> bool ; quux x = length s < 15
```

## My Experience with Idris' Alternatives

I started with a Agda, but the learning resources are aimed at CS grad
students.

I moved on to F\*, which had accessible documentation. But I couldn't
find any intermediate documentation. What I could find was pretty
dense stuff about passing proofs and extracting code to C so you can
write verified crypto functions. F\* is certainly worth another look
if it can do those things, but I just couldn't understand how to use
it.

Finally, having exhausted all other options, I tried Idris, for which
there's [the
book](https://www.manning.com/books/type-driven-development-with-idris)
from. The PDF from Manning press iis DRM-free, so I could me read it
on my open-source e-reader.

I plan to revisit both Agda and F* after I've done a bit more work in
Idris.
