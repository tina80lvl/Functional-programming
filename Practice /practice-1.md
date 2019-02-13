# Practice 1 (February 13th, 2019)

To be executed after lectures 2, 3.

The main goal of the practice is to train skills on using data types and type classes, understand relation and difference.


## Basic data types fun

Define a data type of student's group.

Group has its identifier and students.

Each student has name, surname and date of birth.

Group's data type should be polymorphic over group identifier.
(because university's standards eventually change and we want our codebase to stay resilient to these changes).

Date of birth shall be made with `UTCTime` from `Data.Time.Clock`.

## Beyond Enum class

(Taken from Stepik)

Implement instances of `SafeEnum` class for `Int`, `Bool` and `Char`.

```
class SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a
```

Functions `ssucc` and `spred` are total, behave in cyclic manner.

Example run:

```
GHCi> ssucc False
True
GHCi> ssucc True
False
```

## Small quiz

Without using GHCi answer following questions:

What constraint should be put instead of `?` for us to be able to implement polymorphic sort function?

```
sort :: ? => [d] -> [d]
```

What are key differences between Java's interfaces and type classes?

Hint: can we define `Comparable` for our new data class `A`? Can we attach our new interface `Printable` to existing class `String`?

## Dictionaries and type classes

Hypothesis: ad-hoc polymorphism can be fully emulated with parametric polymorphism.

You're asked to provide an intuitive example of why this seems to be true.

Consider type class `Num` and polymorphic function `f` doing some transformation using methods of this type class:

```
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

f :: Num a => a -> a -> a -> a
f a b c = abs $ negate ((a + b) * c - a) + 234

```

You're asked to encode data type `MyNum` that shall be equivalent to `Num` class.

```
data MyNum a = ...


f' :: MyNum a -> a -> a -> a -> a
f' = ...
```

Is this approach universally applicable?

## Type class inheritance and minimal definition

Implement data type for arithmetics modulo 17.

Implement instance of `Integral` for it.
