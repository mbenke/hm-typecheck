
## Synopsis

Experimental typechecker for weak MPTC, that is multiparameter type classes where one parameter uniquely determines all other.

This is slightly weaker than functional dependencies, but useful in practice, while typechecking is much simpler.

Motivating example:

```
    class ref : Ref[deref] { load : ref -> deref };
    instance Stack[Int] : Ref[Int];
    instance SI : Ref[Int];
    instance Memory[a]: Ref[a] ;
```

Here `Ref[a]` describes references to values of type `a`.

## Quick start

### Building


```
cabal run fun examples/prog1
```

Requires a working GHC installation, see e.g. https://www.haskell.org/ghcup/

### Running

```
cabal run fun examples/mona/01main.fun
```

If the input file has a monomorphic `main` function, all its dependencies are monomorphised and a corresponding Core program is output to `output.core`,
which can be then translated to Yul using `yule`:

```
$ cabal run yule -- output.core
found main
writing output to Output.sol
```
...and run using `forge script`:
```
$ forge script Output.sol
[.] Compiling...
[.] Compiling 1 files with 0.8.23
[.] Solc 0.8.23 finished in 284.26ms
Compiler run successful!
Script ran successfully.
Gas used: 24379

== Logs ==
  RESULT -->  42
```

(see also `yule/README.md`)

## Project structure

### Directories

 * `src` - the typechecker library
 * `fun` - commandline typechecker for a functional IL
 * `examples` - some program examples
 * `yule` - Core to Yul translator

### Modules

### Language/Fun
* `Checker` - central typechecker functions
* `Constraints` - unification and matching
* `ISyntax` - internal syntax - the tree coming from parser is desugared into this form and then fed into typechecker; this allows some independence from concrete syntax
* `NameSupply` - supply of fresh names - not strictly necessary, could be replaced by a counter
* `Prims` - primitive types, functions and classes such as `List` or `Eq`
* `TCM` - the typechecker state and monad
* `Types` - structure of types and predicates for the typechecker
