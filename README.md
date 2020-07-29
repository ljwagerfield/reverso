# Reverso

---

**Functional Constraint Solver for Scala (using Cats Effect).**

---

## Overview

Reverso analyses predicates written in Scala:

```scala
x => x.filter(_ % 2 == 0)
```

And generates inputs to satisfy them:

```scala
LazyList(0, 2, 4, ...)
```


## How it works

Reverso rewrites your predicate into a single-loop function, and then backtracks it.

The resulting single-loop function uses a [primitive AST](/src/main/scala/reverso/PredicateAST.scala) that's been
specifically designed for easy reverse execution: that is, you can start from the end stack frame (i.e. `return true`)
and incrementally fan-out to viable parent stack frames until a valid initial stack frame is found... at which point, a
valid call stack is known, and thus enough information has been gathered about the stack to know what the inputs are.

Reverso uses [Choco](https://github.com/chocoteam/choco-solver) internally to check the compatibility of the
(in)equalities encountered in each new stack frame with the (in)equalities of the prior stack frames. For example, if we
currently know `x > 5` from the prior stack frames, then the parent stack frame cannot assert `x < 2`. The examples get
more complex than this, since our AST supports basic algebra, and expressions can contain multiple variables. Therefore,
the problem statement becomes 'verifying the consistency of multiple systems of multivariate rational (in)equalities' --
and this is what Choco does.

## How is it different?

Reverso's USP was to be a constraint solver with first-class support for complex objects and arrays.

Traditional constraint solvers do not provide first-class support for variable-typed objects or variable-sized arrays:
they support variable scalars only (and since there's no variable-sized arrays, you must define how many scalars you
need upfront). Whilst there are workarounds, they often have limited application. For example, a common workaround
to achieve a variable-sized array is to declare the maximum number of scalars you need upfront, and then a separate
scalar to hold the active length of your array: however, this quickly becomes unwieldy if you need an array of arrays.

Working within these restrictions is like trying to write predicates whereby the inputs can only be complex objects of
fixed structures (i.e. not polymorphic) and must contain only scalars in their fields. If you need anything else (such
as arrays or polymorphic objects), you must implement them using workarounds that adhere to the limitations previously
set forth. It's all doable, but it gets very awkward.

Reverso's intention was to allow you to work with input structures that could be as free as JSON.

## Project status

**ABANDONED.**

### Why was it abandoned?

Poor performance (and lack of any tractable optimisation).

Generating solutions for predicates that use arrays and/or strings as inputs would have resulted in too many variables
and constraints being added to the internal [Choco](https://github.com/chocoteam/choco-solver) model. This is because a
Choco variable is added for each scalar in the predicate's input, and strings (in Reverso) are treated as arrays of
scalars. For example, a predicate to check the equality of two strings would require `N*2` variables, where `N` is the
length of the largest string. Whilst this is feasible for small inputs, the solution doesn't scale, and the problem
compounds as arrays are nested: consider the number of variables required for a predicate that checks if an array of
strings contains any duplicates.

Optimising for these scenarios is impractical since Reverso's design hinges around a predicate AST that is easy to run
in reverse, and the only reason the AST is easy to run in reverse is through the very limited array API that it had
access to: arrays (and strings) only supported `head`, `tail` and `prepend` (not even `size`!). Supporting additional
ways to manipulate and access arrays immediately introduces numerous complexities, for instance, making it harder to
identify if two expressions are actually referring to the same variable. In short, adding first-class support for
strings, and a richer API for arrays (e.g. a `size` function!) would require significant work, or more likely an
entirely different approach.

### The alternative solution (for our use case)

Reverso was originally designed for [AutoSpec](https://github.com/autospec): it was intended to provide input generation
for REST API endpoints through predicates defined by API maintainers.

**The alternative solution we arrived at for AutoSpec** was to design a single AST that is both the generator AST and
the predicate AST. That is, rather than have our users write predicates in a predicate AST, and then effectively
transpile those predicate ASTs to generator ASTs, instead make the two ASTs 'meet in the middle' such that the same AST
can efficiently generate _and_ validate input.
