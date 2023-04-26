The obvious choices:
	- Haskell,
    - PureScript
	- Reason

I started with a Agda, but couldn't get traction with it. The learning
resources are aimed at CS grad students, and it was just too dense for
me. I struggled with the basic syntax, because it's so abstract. I
took a brief detour through `F*`, but found it was similarly beyond my
reach. I exhausted the resources I could find on both of these
languages, and still was struggling to get programs to actually *run*.

Finally, having exhausted all other options, I tried Idris. And I wish
I'd started here. I worked through the online tutorial, and then
decided to buy [the book](TBD: Insert TDD Link) from [Manning
Press]. The PDF was, thankfully, DRM-free and I was able to read it on
my open-source e-reader.

The book was extremely helpful. It's accessible at the expense of
being repetitious. While this is very helpful in the beginning, I
wished that later chapters would focus mainly on the *changes* to the
text, rather than spelling out each step in detail. If you're tempted
to skim ahead, you'll miss crucial details and make mistakes. On the
other hand, some of those mistakes valuable experience in interpeting
diagnostic messages from the type checker.

One big gotcha: the book hasn't been updated for Idris2. There's some
on-line errata, if you want to go that route. Or, you can do what I
did and start with Idris 1. I think I'd have gotten more stuck if I'd
started with Idris 2, because the differences between them are too
subtle for a novice to grok. On the other hand, it was much easier to
install Idris 1 via my distro's packages, vs Idris 2 which I'm now installing via [pack](TBD pack link).

I found Idris was hard for me to learn. Harder than C, C++, Java x86,
mips, 6502 assembly, various flavors of basic, Rust, various dialects
of scheme, various esolangs, and the unix `sed` command. This is why

I've never learned any ML-family languages. They are so different. For
me, it's like starting over from the beginning. I actually had to read
a hundreds of pages of academic research about compiling functional
languages before it even really started to click what the execution
model actually *is*. After a couple years of trying to [create my own
functional language](https://github.com/emsash/udlang], I finally gave
up and decided to learn an existing one.

I started working with Idris in October 2022. It's April 2023 at the
time of this writing, and I've *finally* begun to understand Idris. I
can't say I was working steadily on it -- life has been happening in
the meantime -- but having the patience to work through the book paid
off, and now I finally know what I need to know to be able to write
the Idris port of *rpncalc*, which I'm calling *irpn* for now.

## My Experience with Idris' Alternatives

I started with a Agda, but the learning resources are aimed at CS grad
students. I could barely comprehend the most basic information
about the language.

I moved on to F\*, which had accessible documentation. But I couldn't
find any intermediate documentation. What I could find was pretty
dense stuff about passing proofs and extracting code to C so you can
write verified crypto functions. F\* is certainly worth another look
if it can do those things, but I just couldn't understand how to use
it.

Finally, having exhausted all other options, I tried Idris, for which
there's [a book you can buy](TBD: Insert TDD Link) from [Manning
Press](TBD Manning press). The PDF is DRM-free and this allowed me to
read it on my open-source e-reader.

While the book is for *Idris 1*, they're close enough that I was able
to make the transition to *Idris 2* after having finished.

Anyway, the project has already succeded in that I finally am able to
read *Idris* source, and other flavors of ML are starting to seem a
lot more comprehensible. I hope to write about the experience while
its still fresh.
