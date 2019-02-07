# Object oriented programming

rod's main feature is its native object-orientation: it doesn't have any exotic constructs, like prototypes, or metatables. It's just pure classes.

## Class declaration

To declare a class, use the `class` construct:
```
class MyClass {

}
```

## Class members

### Fields

A field is an instance variable for a class:
```
class Vec2D {
  x, y;
}
```
By default, fields are immutable, which means that their value cannot change after their initial setting.
To make a field mutable, use the `mut` keyword:
```
class Vec2Dmut {
  mut x, y;
}
```

Fields can be static, if initialized with a value:
```
class Greeter {
  lastGreeted = "nobody";
}
```
Static fields are not bound to an instance, but rather to the class itself.
The fact that fields are immutable by default, and static fields are initialized on a class declaration makes them perfect for constants.

All fields are private by default, so getters and setters must be used to allow for outside access.
The compiler can generate those for you if you use the `Encap` pragma:
```
class Vec2D {
  #[Encap]
  x, y;
}
```

### Methods

Methods are class-bound functions:
```
class Greeter {
  mut lastGreeted = "nobody";

  fn greet(person) {
    println("Hello, " + person + "!");
    Greeter.lastGreeted = person;
  }
}

Greeter.greet("World");
```
Normally, a method is always static. However, it can be made into an instance method using `self` as its first parameter:
```
class Greeter {
  mut lastGreeted;

  fn greet(self, person) {
    println("Hello, " + person + "!");
    self.lastGreeted = person;
  }
}

let greeter = Greeter();
greeter.greet("World");
```

### Metamethods

#### `.ctor`

The `.ctor` metamethod specifies a class's constructor. The constructor is called when a class is instantiated.
A constructor must always have a `self` parameter, as it creates an instance. The lack of this parameter is a compile-time error.

```
class Player {
  mut x, y;

  fn .ctor(self, x, y) {
    self.x = x;
    self.y = y;
  }
}
```

Thanks to method overloading, a class can have many constructors:
```
class Player {
  mut x, y;

  fn .ctor(self, x, y) {
    self.x = x;
    self.y = y;
  }

  fn .ctor(self) {
    // the Self class is a metaclass that refers to the current class
    Self(0, 0);
  }
}
```

#### `.dtor`

As opposed to the constructor, the destructor is used to destroy an object and free any memory associated with it.
Normally you don't have to destroy an object explicitly, because Nim's garbage collector will take care of that.
However, in some cases (like game object removal) a destructor may be useful to add some side effects to the object's removal.

A destructor metamethod doesn't have any arguments. There's always one of them per class:
```
class Player {
  fn .dtor(self) {
    game.play_death_animation();
  }
}
```

To destroy an object, a `del` statement is used:
```
let player = Player();
// do something
del player;
assert(player == null);
```
On deletion, the reference to the specific object becomes a null reference.

A destructor is also called upon garbage collection, when the object gets deleted from memory.
However, the moment *when* a destructor is called by the GC is unknown for rod, as it relies on Nim's GC for memory management.
