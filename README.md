# IRPN: A Mobile-First RPN Calculator in [Idris](https://idris-lang.org)

![screenshot!](screenshot.png)

- [x] Single-page app
- [x] Uses reverse-polish notation
	  ([RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation))
- [x] visible stack
- [x] visible variable environment
- [x] Unlimited undo and redo
- [x] Fraction mode
- [x] Mobile-first, responsive layout.

You can try it [here!](https://emdash.github.io/irpn)

## Overview

### Motivation

Two years ago, I wrote [an RPN
calculator](https://github.com/emdash/rpncalc), as an exercise in
minimalist web-programming. *irpn* is a total rewrite in
[Idris](https://idris-lang.org)

The original RPN calculator will remain available, but I no longer
intend to develop it.

### On the Choice of Idris

There were three criteria in chosing Idris:

- It wanted to learn an ML-derived language.
- The language needed to target JavaScript, as I still wanted to
  distribute IRPN as a static single-page app.
- I wanted to try dependent types.

Haskell, PureScript, Reason, and Elm, were ruled out for lack of
dependent types.

You can read about my experience [here](doc/experience.md).

### Browser Support

*irpn* only officially supports Firefox. However, I've made no effort
to prevent you from using *irpn* with other browsers. Your milage may
vary.

If you notice a bug, feel free to file an issue. Better still if you
have a fix.

Most of the core behavior is written in pure Idris, and so shouldn't
depend on the behavior of particular browsers.

One key exception, as ever, is CSS. Getting CSS to work consistently
across browsers is a sisyphean chore and I take no joy in it. Firefox
is my preferred browser, so suporting it is my primary concern.

## Usage

See the [manual](doc/usage.md)

### Roadmap

- [x] get javascript compilation working
- [ ] implement all the functions present in the JS version
  - [x] `approx` for rationals
  - [ ] Coercion between `Double` and `Rat`
  - [ ] Log with arbitrary base
- [x] port UX scaffolding and stylesheets from rpncalc
- [x] implement rendering
- [ ] persist sessions via local storage
- [ ] unit-based arithmetic (dimensional analysis)
- [ ] drag-and-drop stack stack reordering.
- [ ] tape / history editing.
- [ ] user-defined functions.
- [ ] user-defined units and conversions.
- [ ] improve keyboard operation
- [ ] write a blog post about all this
