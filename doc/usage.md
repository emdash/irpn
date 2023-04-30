# Overview

*irpn* uses [reverse polish
notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation)).

When you type digits into the calculator, they first enter an input
register. When you press `Enter`, the contents of the input register
are transfered to the stack.

When you push a function key, operands are consumed from the stack,
with the result being placed atop the stack.

## An example

Let's say you want to evaluate this expression:

*(5 + 4) \* 3*

You would tap the following sequence of keys: `5` `Enter` `4` `+` `3`
`*`.

Now let's say instead you wanted to compute this expression:

*5 + 4 \* 3*

You would tap: `5` `Enter` `4` `Enter` `3` `*` `+`

*irpn* features a visual stack, which I find to be a great help in
keeping track of compuations.

It may seem strange at first, but it has advantages for incremental
computation.

## Auto-Enter

As you may have noticed, a non-empty input register is
automatically transferred to the top of stack. So, either of the
following produces the same result:

- `3` `Enter` `4` `Enter` `+` `5` `Enter` `*`, or equivalently,
- `3` `Enter` `4` `+` `5` `*`

## Unlimited Undo

Another thing to notice are the prominent 'Undo' and 'Redo'
buttons. Every action is undoable, and the size of the undo stack is
unlimited. So be bold, and calculate without fear.

## The swap key

- Sometimes you enter operands in the wrong order. You can use the
  swap key ![swap key!](images/swap.png) to exchange the top two stack
  elements in this case.

## Variables

You can store the top of stack into a variable. With the value you want
to save want to save on top of the stack:
- Switch to `a` or `A` mode.
- Type the name of the variable you want to assign.
- Press the `=` key.
- The variable will appear under the `vars` pane.
- Press the variable in the vars pane to recall the top of stack.
- Alternatively, you can type the name in directly: if the name is
  bound to a value, then it will be placed atop the stack.
- Pressing `Reset` wipes away the stack, register, undo history,
  and any user-defined variables.

## Value Types: Integer, Double, and Rational

These are rendered on the stack in different colors. Light green for
Double-precision float, light blue for arbitrary-precision integers,
and fuscia for rationals.

## Fractional Quantities

The input register supports entering fractions and mixed numbers
directly.

![fractions mode!](images/fractions.png)

When you enter fraction mode (`frac`), the decimal key is replaced
with the fraction key.

The fraction key works like the decimal key: it separates the numerator from the denomiator.

To enter the fraction `3/4`, tap `3` `x/y` `4` `Enter`.

To enter the mixed number `3-3/4`, tap `3` `x/y` `3` `x/y` `4` `Enter`.

Fractions are also automatically created if you take the quotient of
two integers that are not divisible. For example, the `3/4` can also be entered by tapping: `3` `Enter` `4` `divide`.
