# rod TODO list

Those are the features still to be implemented.

## Lexer

 - [ ] Strings
   - [ ] String literal kinds
     - [ ] Single-line
     - [ ] Multi-line
     - [ ] Raw (`r` prefix)

## Parser

### Features

 - [ ] Expressions
   - [ ] Atoms
     - [x] Literals
     - [ ] Variables
     - [ ] Getters
   - [x] Prefix operators
   - [ ] Postfix operators
   - [x] Infix operators

## Compiler

### Features

 - [x] Foreign calls
 - [ ] Variables

## Standard library

The stdlib of rod is going to be lightweight, so only essential functions are going to be featured.

Please do not open issues/pull requests suggesting high-level features like advanced math functions, or GUIs.
If you want those features, create a scripting API for them yourself.

 - [ ] Primitives
   - [ ] `bool`
   - [ ] `num`
   - [ ] `str`
 - [ ] Basic types
   - [ ] class `Fn<T>`
 - [ ] Data structures
   - [ ] class `Vec<T>`
   - [ ] class `Map<T>`
 - [ ] module `concurrent`
   - [ ] class `Fiber`
 - [ ] module `reflect`
   - [ ] class `Module`
   - [ ] class `Class`
   - [ ] class `Method`
   - [ ] class `Field`
   - [ ] class `Pragma`

## Embedding API

The embedding API is going to be split into several layers of abstraction.

### Base rod
Similar to Wren's API.

### autorod
A macro-based solution.
