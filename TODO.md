# rod TODO list

Those are the features still to be implemented.

## Parser

 - [ ] Expressions
   - [x] Operators
     - [x] Prefix
     - [x] Infix
     - [ ] Postfix
       - [ ] Index
     - [x] Assignment
       - [x] Single
       - [x] Tuple
   - [x] Calls
   - [x] `if`
   - [x] Variables and namespaces
   - [ ] Types
     - [x] Primitives
     - [ ] Vectors
       - [ ] Comprehensions
     - [ ] Maps
       - [ ] Comprehensions (?)
     - [ ] Closures (lambdas)
       - [ ] Static typing (?)
 - [ ] Statements
   - [x] Delimiters
   - [x] Expression statements
   - [x] Loops
     - [x] `loop` (sugar for `while true`)
     - [x] `while`
     - [x] `for`
   - [ ] Declarations
     - [x] Variable
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
       - [ ] `impl...for`

## Standard library

The stdlib of rod is going to be lightweight, so only the very basic functions are going to be featured.
Please do not open issues/pull requests suggesting high-level features like advanced math functions, or GUIs.

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
