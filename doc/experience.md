# Overview

I think Idris is a reasonable choice for those looking to understand
dependent types. If you have experience with Haskell, in particular,
then Idris should seem familiar.

If you're like me, and you don't, then you may find Idris quite
challenging. Be patient, and read [the
book](https://www.manning.com/books/type-driven-development-with-idris).

Hopefully, the code in this repo will serve as an example for what a
practical project in Idris might look like.

## On dependent types

I got a taste of dependent typing through systems like
[FlowJS](https://flow.org/) and
[TypeScript](https://www.typescriptlang.org/). Both of these systems
effectively erase (almost) all type information at runtime.

Having the patience to work through [the
book](https://www.manning.com/books/type-driven-development-with-idris)
was crucial. There are a lot of new concepts in Idris, and I needed a
comprehensive guide.

Dependent types enable rich libraries. This can be particularly useful
when it comes to FFI, since Idris' type system can capture the quirks
of the target language.

Dependent types aren't needed everywhere. But when you do find
yourself using them, it starts to feel perfectly natural in Idris.

*Designing* dependent types is a bit more involved.

## What's Great about Idris

- Holes
- Skeleton definition insertion
- Automatic case splitting
- Repl experience
- Type system provides high levels of confidence in code
- Fixes many of [Haskell's warts](https://quentinduval.github.io/blog/2017/06/14/idris-improvements.html)

I plan to expand on these at some point.

# My Path

## Step 0: Install Idris 1

Idris 1 I installed via cabal.

## Step 0.5: Configure your editor

There's an apendix in *The Book* which documents configuration of the
Atom editor. For emacs, you'll want to intall `idris-mode` via melpa
or elpa, and then customize it to launch `idris2` instead of `idris`.

### Emacs Cheat Sheet

The book doesn't give emacs keybindings, using the Atom keybindings
throughout. Here's a cheat sheet with Atom keybindings mapped to their
emacs equivalents:

| Atom Keybinding | Emacs Keybinding | Meaning                                                                      |
|-----------------|------------------|------------------------------------------------------------------------------|
| `Ctrl + Alt A`  | `C-c C-s`        | Add skeleton definition                                                      |
| `Ctrl + Alt C`  | `C-c C-c`        | Case split                                                                   |
| `Ctrl + Alt D`  | `C-c C-d d`      | Display documentation for symbol under cursor                                |
| `Ctrl + Alt L`  | `C-c C-e`        | Lift hole to top-level definition                                            |
| `Ctrl + Alt M`  | `C-c C-c`        | Replace hole with case expression                                            |
| `Ctrl + Alt R`  | `C-c C-l`        | Reload and type-check                                                        |
| `Ctrl + Alt S`  | `C-c C-a`        | Searches for an expression which satisfies the type of the hole under cursor |
| `Ctrl + Alt T`  | `C-c C-t`        | Display type of symbol under cursor                                          |
| `Ctrl + Alt W`  | `C-c C-w`        | Add a with block under current line                                          |

*Note:* `C-c C-c` has two functions, this isn't a typo.

[Source](https://github.com/idris-hackers/idris-mode#interactive-editing)

The emacs mode can do a lot more, but this should get you through the
book.

### Vim Cheat Sheet

I don't use vim, but am happy to include vim-specific tips in this
section if someone contributes them.

## Step 1: Read *The Book*

[The
book](https://www.manning.com/books/type-driven-development-with-idris)
is essential.

It's accessible at the expense of being somewhat repetitious. But if
you're new to ML-family syntax, you should work through the first 9 -
11 chapters at least.

I wished that later chapters presented the *changes* to each listing
in some typographic form, rather than spelling out editing step in
detail. Often I would get the gist of the material, but the format
made it hard to spot the changes at a glance -- and was a bit
tedious. All I can say is: just be patient.

Work through each exercise! Get used to the interactive editing
commands. After a few chapters, you should accumulate sufficient
muscle memory to not need the cheat sheet.

The biggest gripe I have is that the book still assumes Idris 1. Idris
2 has been around for a few years at this point, and Idris 1 is
essentially dead. So, while there is a [list of
changes](https://idris2.readthedocs.io/en/latest/typedd/typedd.html)
available online, I felt it was too much cognitive load to start with
Idris 2. So I used Idris 1 until I was done with the book. But I
really wish they'd do an Idris 2 edition.

In the mean time, there is [stephan hoeck's
tutorial](https://github.com/stefan-hoeck/idris2-tutorial) which is a
promising introduction to Idris 2. I'll revisit it some day soon.

## Step 2: Migrate to Idris 2

I installed Idris 2 via
[pack](https://github.com/stefan-hoeck/idris2-pack).

If you install this way, you can also use pack for dependency
management in your idris projects, which is important because Idris
has a relatively small standard library.

Idris2's standard library is much more complete than that of Idris 1,
so this is a good incentive to migrate to Idris 2 as soon as you get
far enough into the book.

# Bugs, Gotchas, and Advice

What follows is a list of things I struggled with, in no particular
order.

In general, I think it's fair to say that Idris 2 is not
production-ready. However, it is *usable*.

## Minor nits

- In the emacs mode, I have to type-check twice the first time I load
  a file.
  - the first time, it complains it can't find the file.
  - just hit it again, and it works after that.
- I manage to wedge my editor session somewhat regularly.
  - If things start acting weird, try restarting your editor before
    you do anything drastic with your code.
- It's possible to write code that type-checks, but can't be executed.
  - So run your code often!
- It's possible to define types that you cannot implement.
  - either because it's impossible, or because you're not smart enough
  - write your types in parallel with some representative uses, to
    spot this early.
- Compiler error messages can be inscrutable and misleading.
  - If an error doesn't make sense, it might be a compiler issue.
  - Often the error message will highlight the wrong portion of the source code

### Misleading Error Message Example

Consider the following idris source:

```Idris
||| Apply f on the cross-multiplication of a and b
withCrossMul : Rat -> Rat -> (Integer -> Integer -> a) -> a
withCrossMul a b f = f (a.num * cast b.denom) (b.num * cast a.denom)

||| Add two rational numbers
public export
add : Rat -> Rat -> Either String Rat
add a b = rat (withCrossMul a b (+)) (a.denom * b.denom)

||| Subtract two rational numbers
public export
sub : Rat -> Rat -> Either String Rat
sub a b = rat (withCrossMul a b (-)) (a.denom * b.denom))
```

The compiler reports the following error message:

```
- + Errors (1)
 `-- src/Rat.idr line 90 col 0:
     Couldn't parse declaration.

     Rat:90:1--90:4
      86 |
      87 | ||| Subtract two rational numbers
      88 | public export
      89 | sub : Rat -> Rat -> Either String Rat
      90 | sub a b = rat (withCrossMul a b (-)) (a.denom * b.denom))
           ^^^
```

This is misleading. The actual problem is at the end of the line:

```
90 | sub a b = rat (withCrossMul a b (-)) (a.denom * b.denom))
                                                             ^ extra parenthesis
```

Consider this when trying to resolve a seemingly nonsensical error.

## Practical Advice

- Write a .ipkg for your project early
  - you get this for free if you use pack to create new projects
  - You probably want to add the `contrib` package to your
    dependencies.
- make it a habbit to place `%default total` in all your files.
  - this makes all definitions implicitly total, and you opt into partiality
  - this is better than the default, which is the other way around
  - only total functions can be normalized when used in types!
  - if a function is partial, it can crash at runtime, or fail to terminate!
- Do all the exercises in the first few chapters of *The Book*.
  - Type everything in yourself!
  - Practice the keystrokes, as they are *key* to being productive!
- You can comment out to the end of the file with `{-`,
  - which is handy for figuring out where the error *really* is.
  - Idris allows nested comments, so this always works!
- Check into source control often.
- Use [idris2-pack](https://github.com/stefan-hoeck/idris2-pack) to
  create your project and manage its dependencies.
- Compile your whole project often via `pack build`
  - Interactive type-checking only checks the current file and its
    dependencies.
  - You can break code in other files, and not realize it unless you
    happen to switch files.
  - So compile your whole project often.
- Lean on the repl to check and run your code quickly
- use holes to type-check incomplete files

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
