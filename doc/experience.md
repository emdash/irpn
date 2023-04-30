# Overview

My primary motivation is to understand dependent types. I think Idris
is a reasonable choice for those looking to understand types. If you
have experience with Haskell, in particular, then Idris should seem
familiar. Idris is more accessible than the alternatives I've tried so
far.

Although I studied Computer Science at university, most of my
exeprience is in industry and open-source development. My exposure to
FP is mainly with LISP -- in particular, elisp and scheme, and of
course modern JS, with a bit of Rust and Scala in recent years. So I
have some intuition for ML type systems, but I'd never used any
ML-family langauges before starting this project.

I've tried to learn Haskell a few times over the years, but I could
never find an appropriate resource aimed at industry veterans. Most of
the resources out there assume you're either a rank beginner, or a
grad student in PL research. I didn't have the time or inclination to
work through either type of resource.

The the situation isn't much better in Idris, but the lure of
dependent types made it finally worth the pain of learning something
very different from what I'm used to.

## On dependent types

I got a taste of dependent typing through systems like [FlowJS](TBD)
and [TypeScript](TBD), which also offer some limited form of refinemnt
and structural typing. Both of these systems effectively erase all
type information at runtime, so while types may depend on terms, terms
may not depend on types. And while type-level functions are supported,
FlowJS, for example only supports a small handful of type-level
primitives. Neither one offers full dependent typing.

At this point, I'm definitely *using* dependent types, but I can't say
I'm *leveraging* them to any good effect. Idris's type system is so
powerful, one can avoid them altogether and not feel like anything is
missing.

I expect this will change as my understanding improves.

If you're used to Haskell, then you should feel right at home in
Idris. On the other hand, If you've *never* used an ML-family
language, then you may find Idris quite challenging. I started working with
Idris in October 2022, it's April 2023 at the time of this writing --
, but I've *finally* begun to understand Idris.

Having the patience to work through [the
book](https://www.manning.com/books/type-driven-development-with-idris)
paid off, and this project is proof that I'm learning how to be
productive.

## What's Great about Idris

- Holes
- Skeleton definition insertion
- Automatic case splitting
- Repl experience
- Type system provides high levels of confidence in code
- Fixes many of [Haskell's warts](TBD)

I plan to expand on these at some point.

# My Path

## Step 0: Install Idris

Idris 1 I installed via cabal.

Idris 2 I install via [pack](TBD).

### Step 0.5: Configure your editor

There's an apendix in *The Book* which documents configuration of the
Atom editor. For emacs, you'll want to intall `idris-mode` via melpa
or elpa, and then customize it to launch `idris2` instead of `idris`.

#### Emacs Cheat Sheet

[The
Book](https://www.manning.com/books/type-driven-development-with-idris)
uses Atom's key bindings, but I prefer emacs. There is an emacs mode
for Idris, but it uses different key-bindings from Atom. And the key
bindings use different naming.

I had to look on google until I found the following [cheat
sheet](TBD), which I'll summarize here:

TBD

## Step 1: Read *The Book*

[The
book](https://www.manning.com/books/type-driven-development-with-idris)
was essential.

It's accessible at the expense of being somewhat repetitious. But if
you're new to ML-family syntax, you're not going nowhere without it.

I wished that later chapters presented the changes to each listing in
some condensed form, rather than spelling out each step in
detail. Often I would get the gist of what was happening, but the
format made it hard to spot the changes at a glance. All I can say is:
just be patient.

Especially in the beginning, work through each exercise. Get used to
the editor and its interactive features. After a few chapters,
however, you should accumulate sufficient muscle memory.

The biggest gripe I have is that the book still assumes Idris 1. And
while there is a [list of changes](TBD) available online, I felt it
was too much cognitive load. So I stayed with Idris 1 until I'd
essentially finished *The Book*.

In the mean time, there is [stephan hoeck's tutorial](TBD) which is a
promising introduction to Idris 2. I started working through it, but
there was too much overlap with what I'd already covered in *The
Book*. I'll revisit it some day soon.

## Step 2: Migrate to Idris 2

TBD

## Step 3: Learn Pack

TBD

# Bugs, Gotchas, and Advice

What follows is a list of issues, nits, and thinks I struggled with,
in no particular order.  I've spent a lot of time on the fringes of
computing, so nothing in here is a deal-breaker for me,
personally. But consider how much these would affect you before
choosing Idris.

In general, I think it's fair to say that Idris 2 is not
production-ready. I wouldn't entrust it to mission-critical or
financially significant roles at ths point in time.

However, it is *usable*, and I think in particular it's a good choice
for an early-stage open-source project whose developers want something
a bit like Haskell, but more streamlined.

## Minor nits

- I manage to wedge my editor session somewhat regularly. If things
  start acting weird, try restarting your editor before you do
  anything drastic.
- It's possible to write code that type-checks, but can't be executed.
  - So run your code often!
- It's possible to define types that you cannot implement.
  - either because it's impossible, or because you don't know how to
    write the necessary proofs.
  - write your types in parallel with some representative uses, to
    spot this early.
- in the emacs mode, I have to type-check twice the first time I load
  a file.
  - the first time, it can't seem to find the file.
- Compiler error messages can be inscrutable and misleading.
  - If an error doesn't make sense, it might be a compiler issue.

## Practical Advice

- Do all the exercises in the first few chapters of *The Book*.
  - Type everything in yourself!
  - Practice the keystrokes, as they are *key* to being productive!
- You can comment out to the end of the file with `{-`,
  - which is handy for figuring out where the error *really* is.
  - Idris allows nested comments, so this always works!
- Check into source control often.
- Compile your whole project often
- Use [idris2-pack](TBD) to create your project and manage its dependencies.
- Test and run your code in the REPL often.

## Emacs Cheat Sheet

## My Coding Style

See [the style guide](style.md)

# Idris' Alternatives

## Agda

I started with a Agda, but the learning resources are aimed at CS and
PLT grad students.

The main thing I disliked about Agda is that, since it allows unicode
in identifiers, and it allows arbitrary user-defined mixfix operators,
people go crazy with this and try to make their source code look like
the output of a LaTeX file.

But the real issue is just that all of the material I could find on
the language was really aimed at people who wanted to write proofs,
even though its just as much of a general-purpose programming language
as Idris. All the examples are drawn from graduate-level PL theory and
mathematics. I just didn't have a leg to stand on. Having learned
Idris, I plan to take another look at Agda.

## F*

I moved on to F\*, which at least offers accessible introductory
materials.

But I couldn't find any intermediate documentation. Again, what I
*was* able to find was fairly dense stuff: in this case, aimed at
writing verified low-level cryptographic routines.

What's interesting about F\* is that a subset of it can be extracted
to C. So you can write, for example, some tricky array processing
function, passing around a bunch of erased proofs that you never
access outside the array bounds. And the result is converted to
somewhat readable C, which you can run basically anywhere. Wouldn't
*that* be useful?

F\* also has a very convenient notation for refinement types.

It's not clear to me if F\*'s type system is as powerful as Idris' or
Agda's, but it too seems worth another look.
