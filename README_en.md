CX Language Interpreter
===========================================

### Installation

* Compile using 'make' command
* I usually use cabal, I failed to make it work on students however
* Running 'good' tests showing what the interpreter is capable of using 'make test_good'
* Running 'bad' tests causing different kinds of errors using command 'make test_bad'
  (no newlines, slightly difficult to track)
* Program can be run in vocative mode (-v)


### General description

Language similar to C, with certain number of aesthetical and practical differences.
* Data types: Bool, Int, String (upper case)
* In _for_, _while_ and _if_ brackets '()' are not needed
* In _for_ loop expressions are separated by commas ','
* Expressions after loops and conditions are *always* surrounded by brackets '{}'
* No pointer data types


### Project requirements

* datatypes - Int, String, Bool
* arithmetics, comparisons, a whole lot of operators
* loop and condition statements (_while_, _for_, _if_, _if-else_)
* built-in functions: print, to/from string conversion
* variable shadowing and static binding
* static typing
* runtime error handling
* arguments in functions as value or reference


### Expressions interpretation

Variables are treated as _lvalue_, meaning values that can be modified.
Within the code of interpreter they are used as references.

That allows, among other things, chaining assignment operators:
`a = b = 5`


### Type checking

Type checking is made to work on values; an attempt to assign value of type Int to _rvalue_
will end up in a runtime error, rather than type error.

Other than that, everything else is checked: instructions, expressions, global and local
declarations, function arguments shadowing.

Just like in C, execution of the program starts in a function called 'main' of type
Int.


### Built-in functions

* String to/from Bool/Int conversion functions
* 'print' function (Logical values are intentionally printed "True", "False", while
  in the language logical constants are _true_ and _false)


### Intended features (not yet implemented)
* Passing functions as arguments
* Tuples functioning as regular types
* Functions as _return-value_


### Files

#### Code
* EBNF/CX.cf - grammar
* Main.hs - main program
* CXBase.hs - auxiliary functions and declarations
* CXTypeChecking.hs - surprisingly, type checking

#### Tests: Good
* testProg.cx - general test program
* basicConstructs.cx - _for_, _while_, _if_, _if-else_
* operators.cx - arithmetical and logical operators
* builtinFunctions.cx - built-in functions
* recursiveFunction.cx - simple recursive funcion
* variableShadowing.cx - variable shadowing plus references

#### Tests: Bad
* badForLoop.cx - parsing error due to C-style _for_ loop with colons ';'
* builtinFunctionArg.cx - type error in string conversion function arguments
* incorrectFunctionArgs.cx - type error in user function arguments
* functionArgsNumber.cx - type error in the number of function arguments
* operatorArg.cx - type error in operator arguments
* mainReturn.cx - type error due to lack of return value in 'main' function
* zeroDivision.cx - runtime error when dividing by 0
* rvalue.cx - runtime error when trying to assign value to an _rvalue_
