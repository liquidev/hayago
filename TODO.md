# rod TODO list

Those are the features still to be implemented.

## Compiler

### Features
 - [ ] Expressions
   - [ ] Operators
     - [x] Prefix
     - [ ] Infix
     - [ ] Postfix
       - [ ] Index
     - [ ] Assignment
       - [ ] Single
       - [ ] Tuple
   - [ ] Calls
   - [ ] `if`
   - [ ] `do {}`
   - [ ] Variables and namespaces
   - [ ] Types
     - [ ] Syntax
     - [ ] Primitives
     - [ ] Vectors
       - [ ] Syntax
       - [ ] Comprehensions
     - [ ] Maps
       - [ ] Syntax
       - [ ] Comprehensions (?)
     - [ ] Classes
       - [ ] Syntax
       - [ ] Shorthand
     - [ ] Closures (lambdas)
       - [ ] Syntax
       - [ ] Static typing (?)
 - [ ] Statements
   - [ ] Delimiters
   - [ ] Expression statements
   - [ ] Loops
     - [ ] `loop` (sugar for `while true`)
     - [ ] `while`
     - [ ] `for`
   - [ ] Declarations
     - [ ] Variable
       - [ ] Static typing
     - [ ] Function
       - [ ] Variadics
       - [ ] Static typing
     - [ ] Class
       - [ ] Fields
       - [ ] Methods
         - [ ] Static
         - [ ] Instance
       - [ ] Operator overloading
     - [ ] Trait
       - [ ] `trait`
       - [ ] `impl…for`
   - [ ] `use`

## Standard library

The stdlib of rod is going to be lightweight, so only the very basic functions are going to be featured.

Please do not open issues/pull requests suggesting high-level features like advanced math functions, or GUIs.
If you want those features, create a scripting API for them yourself.

 - [ ] Primitives
   - `num`
   - `str`
   - `fn<T>(...args)`
 - [ ] Data structures
   - [ ] class `Vec<T>`
   - [ ] class `Map<T>`
 - [ ] module `concurrency`
   - [ ] class `Fiber`
 - [ ] module `reflect`
   - [ ] class `Module`
   - [ ] class `Class`
   - [ ] class `Method`
   - [ ] class `Field`
   - [ ] class `Pragma`

## Embedding API

The embedding API is going to be split into several layers of abstraction.

### Plain, old rod
Similar to Wren's API.

### autorod
A macro-based solution.
