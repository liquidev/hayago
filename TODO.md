# rod TODO list

Those are the features still to be implemented.

## Parser

### Source code
 - [ ] Use a `pegs`-like DSL for AST generation

### Features
 - [ ] Expressions
   - [x] Operators
     - [x] Prefix
     - [x] Infix
     - [x] Postfix
       - [x] Index
     - [x] Assignment
       - [x] Single
       - [x] Tuple
   - [x] Calls
   - [x] `if`
   - [ ] `do {}`
   - [x] Variables and namespaces
   - [ ] Types
     - [x] Syntax
     - [x] Primitives
     - [ ] Vectors
       - [x] Syntax
       - [ ] Comprehensions
     - [ ] Maps
       - [x] Syntax
       - [ ] Comprehensions (?)
     - [x] Classes
       - [x] Syntax
       - [x] Shorthand
     - [x] Closures (lambdas)
       - [x] Syntax
       - [x] Static typing (?)
 - [ ] Statements
   - [x] Delimiters
   - [x] Expression statements
   - [x] Loops
     - [x] `loop` (sugar for `while true`)
     - [x] `while`
     - [x] `for`
   - [ ] Declarations
     - [x] Variable
       - [x] Static typing
     - [x] Function
       - [ ] Variadics
       - [x] Static typing
     - [x] Class
       - [x] Fields
       - [x] Methods
         - [x] Static
         - [x] Instance
       - [x] Operator overloading
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
