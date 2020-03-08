# HW3: Implementing the Ruse language

In this homework, you'll implement a programming language called Ruse. We've
given you some starter code, but you'll be doing almost all of the implementation
work. Along the way, you'll get hands-on experience working with the
Functor/Applicative/Monad typeclasses in a complex application.

# About Ruse

Ruse is a simple example of the Lisp family of programming languages; modern
languages in this family include Scheme, Racket, and Clojure. At its heart, Ruse
is an untyped lambda calculus with useful extensions and different syntax. A
language-specification for Ruse can be found at `ruse-spec.pdf`---you'll want
to refer back to this document often to understand what your implementation
should do. The language specification includes a grammar describing the syntax
of Ruse, and a big-step operational semantics describing how Ruse programs
should be evaluated. If any aspects of the specification are unclear, please let
us know ASAP.

# Part 0: Starter code (`src/Ruse/Syntax.hs`)

Most of the starter code is in the `Syntax.hs` file. Here, we define a type for
the abstract syntax tree of Ruse programs (`RExpr`), and we provide some useful
functionality like pretty-printing (`ppRExpr`) and substituting expressions into
other expressions (`subst`). You should familiarize yourself with these
functions, but you should not need to make any changes to them.

Important note: the AST includes a case `Cond` for conditional expressions.
This is an **optional** feature, and you should ignore `Cond` in all of your
code---we won't test this feature unless you want to do the extensions.

# Part 1: Big-step evaluator (`src/Ruse/Eval.hs`)

First, you will build a big-step evaluator for Ruse. This will take a Ruse
expression and return either the final value it steps to (another Ruse
expression, or an error string. You'll want to read the semantics section of the
language specification carefully to see what your evaluator should do. Along the way,
you'll get plenty of experience working with the `Either String` monad.

We've started things off by giving you the case for evaluating `Plus`. However,
the code is written in poor style---your first task is to rewrite `evalPlus`
using `do`-notation. You should be able to remove the first two case expressions.
Then, you should complete the rest of the cases. See the Testing section below
to see how to test your evaluator; you can also test your code in `ghci`.

One last note about the evaluator. It is possible to write non-terminating
programs in Ruse. You should not try to do anything fancy to detect this
possibility---if a non-terminating Ruse program is given to the evaluator, the
evaluator should simply loop.

# Part 2: Parsing the language (`src/Ruse/Parser.hs`)

Second, you will build a parser for Ruse. We have given you a parser type and
some basic utility functions. You have two main tasks.

## Part 2a: Implementing a parser

As a first step, you will implement basic ways of combining Parsers. These will
make `Parser` into a `Functor`, `Applicative`, `Monad`, and an `Alternative`.
We've written the instances for `Functor` and `Applicative` for you, so you just
need to complete `Monad` and `Alternative`. Then, you'll define basic
building-block parsers like we discussed in class for parsing a single
character, parsing a space, etc.

## Part 2b: Parsing Ruse

With the parser in hand, you will build a parser for Ruse. One straightforward
strategy is to implement a parser for each kind of AST node, and then use a big
choice (<|>) over all parsers. Take a look at the comments---we've implemented
some of the cases for you.

The `Lam` and `Rec` case are trickier---pay close attention to the comments.

# Part 3: Implementing a REPL (`app/Main.hs`)

Finally, you will build a simple interactive REPL for users to write Ruse
programs. Along the way, you'll get a bit of experience in working with the IO
monad. Take a look at the comments in this file to figure out what to do. This
is the smallest part of the assignment.

# Testing (`app/Tests.hs`)

We've included some basic tests for your evaluator and parser in this file. To
run these tests, you can use the Haskell repl (`cabal v2-repl`), load
the Tests module (`:l Ruse.Tests`), and then run your test
(`testEval`/`testParse`).

There's further **optional** material here, if you want to get experience with
Quickcheck (the fourth monad). We've written a basic expression generator, which
can randomly generate Ruse expressions. The basic generator doesn't cover many
cases, but you can extend the generator to cover more of the AST. Once you have
a generator, you can plug it in with the provided properties in `ghci` to check
the property on 100 random tests. The `printParse` property can be useful for
testing your parser more thoroughly.

# Extension: variable-length things (OPTIONAL, 5 pts)

This optional extension is worth at most five extra points. It's likely to be a
lot of effort though, so you should only attempt it if everything else is
working and you're interested in doing more. There are three main tasks, and you
don't need to do all of the tasks to receive partial credit.

## Conditional expressions

Ruse has conditional expressions, much like other Lisp languages. Conditional
expressions have the following syntax:

```
(cond ((e1 e1') (e2 e2')... (en en')) (else e))
```

where all `e` are expressions.

That is, there is a list of pairs `(e e')` where the components of each pair are
separated by one-or-more spaces, and each pair is separated by one-or-more
spaces, and there is a final pair of the form `(else e)`. There may be zero
pairs, in which case the expression looks like this:

```
(cond () (else e))
```

To evaluate conditional expressions, the idea is that in each pair, the first
element should be a boolean-valued guard expression. Evaluation should evaluate
each pair---in order---until a guard expression evaluates to `#t`. The
conditional expression should then step to the second component of that pair. If
no guard evaluates to true, the expression steps to the body of the `else`
branch. For instance, the following expression steps to 5:

```
(cond (
  ((eq? 1 2) 4)
  ((eq? 1 1) 5))
  (else 6))
```
Linebreaks have been added for readability, but they are not needed.

Extend the evaluator and parser to handle conditional expressions. You should
use the `Cond` AST node we have given you in `RExpr`.


## Variable-length operations

Many of the arithmetic and boolean operations in Ruse can be extended to handle
multiple arguments, like in Scheme:

```
(+ 1 2 3)
(and #t #t #f #t)
```

Extend the parser to support variable-length argument lists for Plus, Mult, And,
and Or. (Note: you *don't* need to extend Subt.) Rather than extending the AST,
your parser should convert multiple-argument calls into nested two-argument
calls. For instance:

```
(+ 1 2 3 4)
```

should parse to

```
Plus (NumC 1) (Plus (NumC 2) (Plus (NumC 3) (NumC 4)))
```

Then, your evaluator should continue to work as before. This shows the benefit
of *syntactic sugar*: by defining new syntax in terms of the existing AST, we
only have to modify the parser. All other operations on the AST will work
without any changes.

## Let-bindings

Scheme has let-bindings to declare new local variables. These have the following
syntax:

```
(let ((var1 e1) (var2 e2) ... (varn en)) e)
```

The idea is the pairs bind the variables `var1`, ..., `varn` to `e1`, ..., `en`,
and the body expression `e` can mention `var1`, ..., `varn`. Also, later
bindings can mention variables from earlier bindings.

Extend the parser to handle let-bindings. Again, you should not need to modify
the evaluator: the let-binding

```
(let ((x e1) (y e2)) e)
```

is syntactic sugar for nested functions and applications:

```
((lambda x ((lambda y e) e2)) e1)
```
