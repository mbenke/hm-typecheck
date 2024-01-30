
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
cabal run fun examples/prog1
```

## Project structure

### Directories

 * `src` - the typechecker library
 * `cli` - commandline typechecker
 * `examples` - some program examples

### Modules

* Checker - central typechecker functions
* Constraints - uification and matching
* ISyntax - internal syntax - the tree coming from parser is desugared into this form and then fed into typechecker; this allows some independence from concrete syntax
* NameSupply - supply of fresh names - not strictly necessary, could be replaced by a counter
* Prims - primitive types, functions and classes such as `List` or `Eq`
* TCM - the typechecker state and monad
* Types - structure of types and predicates for the typechecker