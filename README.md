# Mobile-First RPN Calculator written in Idris

![screenshot!](images/screenshot.png)

- Uses reverse-polish notation ([RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation))
- visible stack
- visible variable environment
- Unlimited undo and redo
- Fraction mode
- Single-page
- Mobile-first, responsive layout.

You can try it [here](https://emdash.github.io/irpn) (note: live demo not available)

## Overview

### Motivation

This is an idris port of my original pure JavaScript calculator,
available [here](https://github.com/emdash/rpncalc)

I'd been itching to re-write *rpncalc* in a langauge with strong
types. I hope that using Idris will allow me to add advanced features.

The original RPN calculator will remain available, but suffers from a
lack of attention on my part. I pushed the implementation as far as
modern JS would let me, and then started losing interest.

Of course, the main goal is to help me learn Idris by doing something
something non-trivial with it.
  
### Caveats

At present, only Firefox is supported, because that is my primary
browser.

## Usage

This section is TBD. For now, refer to the readme from [my previous
project](https://github.com/emdash/rpncalc)

### Roadmap

- get javascript compilation working
- implement all the functions present in the JS version
- port UX scaffolding and stylesheets from rpncalc
- implement rendering
- persist sessions via local storage
- unit-based arithmetic (dimensional analysis)
  - us customary
  - SI
  - CGS
- drag-and-drop stack stack reordering.
- tape / history editing.
- user-defined functions.
- user-defined units and conversions.
- improve keyboard operation
- write a blog post about all this
