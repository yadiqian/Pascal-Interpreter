# Pascal Interpreter in Haskell with Happy and Alex

Author: Yadi Qian

The following are implemented in this project:

* Program Name (no args)
* Variable Declarations
* Main program block
* Comments: (* ... *) and //
* Basic arithmetic expressions with variables
* Boolean/logical Expressions
* Precedence of operators
* Decision Making (if-then-else, case)
* Special Expressions: Readln, Writeln, sqrt, sin, cos, ln, exp
* While-do and for-do loops
* User-defined procedures and functions
* Variable scoping

## Commands
To run e2e tests
```
cabal run /tests/test1.pas
```
Note that there are 20 e2e test files in the ```tests``` folder.

To run unit tests:
First cd into the ```src``` directory with 
```
cd src
```
then run
```
runhaskell spec/FunctionSpec.hs; runhaskell spec/InterpretSpec.hs; runhaskell spec/ScopeSpec.hs
```
All the unit test files reside inside the ```Spec``` directory.

## Notes
My interpreter is also able to handle
* nested while and for loops
* recursion
