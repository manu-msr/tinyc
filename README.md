# TinyC

Manuel Soto Romero

This repository documents the incremental design and implementation of
**tinyC**, a pedagogical imperative language conceived to study mutable state,
sequential execution, and structured control. Each version preserves the
previous core and adds only the constructs needed for the next semantic
problem.

The project uses **Haskell** as the host language. Alex and Happy describe the
concrete syntax, Haskell data types represent the abstract syntax, and direct
interpreters make the dynamic semantics executable.

tinyC uses **natural operational semantics** as its executable model. A CES
configuration separates control, an environment that maps identifiers to
locations, and a store that maps locations to integers. Arithmetic
expressions inspect this state without modifying it; commands receive an
environment and a store and produce their final state. The Haskell
interpreters mirror this big-step specification directly.

## Repository Contents

### Sequential Programs

* [tinyC v1](TINYC01): Arithmetic expressions, initialized declarations,
  assignments, and command sequences.

### Structured Control

* [tinyC v2](TINYC02): Extends the sequential language with integer
  comparisons, logical negation, decision through `if`, and repetition
  through `while`.

## Purpose

The overarching aim of this repository is to provide a **didactic progression**
from arithmetic evaluation to imperative control. Through its two versions,
students and researchers are invited to analyze:

* the correspondence between concrete syntax and abstract syntax,
* the distinction between expressions and commands,
* the separation of identifiers, locations, and stored values,
* the propagation of state through sequential composition,
* integer conditions and the behavior of `if` and `while`, and
* the correspondence between big-step rules and direct interpreters.

By expressing the formal semantics as small Haskell interpreters, the project
serves as a teaching tool for connecting language definitions, derivations,
and executable behavior.
