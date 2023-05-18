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
  *swap key* (`⬍`) to exchange the top two stack elements in this
  case.

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

When you enter fraction mode (`frac`), the decimal key is replaced
with the fraction key.

The fraction key works like the decimal key: it separates the
numerator from the denomiator.

To enter the fraction <tt>$3 \over 4$</tt>, tap `3`
<tt>$x \over y$</tt> `4` `Enter`.

To enter the mixed number <tt>$3 {3 \over 4}$</tt>, tap `3`
<tt>$x \over y$</tt> `3` <tt>$x \over y$</tt> `4` `Enter`.

Fractions are also automatically created if you take the quotient of
two integers that are not divisible. For example, the
<tt>$3 \over 4$</tt> can also be entered by tapping: `3` `Enter` `4` `÷`.


### Simplification and Approximation

Addition of fractions uses cross-multiplication, resulting in
ever-larger denominators.

To express a fraction in lowest terms, use the `Simplify` key:

| Keys                                     | Result Stack           |
|------------------------------------------|------------------------|
| `5` <tt>$x \over y$</tt> `1` `2` `Enter` | <tt>$5 \over 12$</tt>  |
| `3` <tt>$x \over y$</tt> `8` `+`         | <tt>$76 \over 96$</tt> |
| `simplify`                               | <tt>$19 \over 24$</tt> |

You can also approximate to an arbitrary denominator. The `≈` function
takes a fraction and a positive integer which represents the
denominator:

| Keys                                     | Result Stack           |
|------------------------------------------|------------------------|
| `5` <tt>$x \over y$</tt> `1` `2` `Enter` | <tt>$5  \over 12$</tt> |
| `3` <tt>$x \over y$</tt> `8` `+`         | <tt>$76 \over 96$</tt> |
| `1` `0` `≈`                              | <tt>$8  \over 10$</tt> |
| `simplify`                               | <tt>$4  \over  5$</tt> |

## Words, Variables, and Functions

In addition to entering numbers and applying functions, you may also
enter characters into the keypad. The input register must be empty. If
the next input is an alphanumeric character, then the input register
contains a word.

### Qwerty Layouts: `a` and `A`

The `scientific` layout has two handy character keys, 'x' and 'y'. These
behave just like `x` and `y` in the `a` qwerty keypad layout.

Choose the `a` layout to enter lower-case letters. Choose the `A`
layout to enter upper-case letters.

The contents of the `Vars` pane is filtered whenever the accumulator
contains a valid word.

### Storing Values

If the input register contains an unknown word, then pressing enter
will place the word onto the stack.

The top of the stack should contain the synbol being defined, with the
value to be assigned directly below:

| Keys        | Result Stack | Environment |
|-------------|--------------|-------------|
| `5` `Enter` | `5`          |             |
| `x` `Enter` | `x` `5`      |             |
| `=`         |              | `x: 5`      |
| `x` `Enter` | `5`          | `x: 5`      |

You can also tap `x` directly within the environment to recall x.

Variables remain bound until the calculator is reset. It is not
possible to re-define a variable once bound, because typing its name
recalls its value.

### Words are Functions

Variable bindings are a special case of function: constant
functions. In general, functions can make arbitrary changes to the
stack.

Typing the name of a function applies the function to the current
stack. Every function key is bound to a named function. Tapping this
key is equivalent to typing the function and pressing enter.

The symbol shown on the function key is not necessarily the same as
the function name. For example, `√` is bound to a function named
`sqrt`. The Vars pane shows both the function name and its
corresponding symbol, if any exists.
