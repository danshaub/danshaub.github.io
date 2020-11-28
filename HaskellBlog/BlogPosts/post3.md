[Blog Hub](../index) | [Previous](post2) | [Next](post4)

# Post 3<br>Abstract and Recursive Data types in Haskell

## Haskell's Data Typing System

<br>

Haskell allows for very flexible data typing when compared to other languages. In essence, it's possible to define your own new kind of primitives; not quite objects, as in languages like Java and C++ (although something more similar is possible with type classes), but distinctly their own thing, more similar to structs. These are often called Abstract Data Types or Algebraic Data Types (ADT for short)

Type declarations are initiated with the `data` keyword and are followed by the name of the data type and what the type "looks" like. Let's take a look at an example:

    data Rectangle = float float

Translated into engilsh, this is saying something to the effect of "let there be some data type 'Rectangle' that stores two floating point numbers representing the length and the width of the rectangle"

That should feel pretty familiar coming from imperative languages. An analogue in C++ would look something like:

    struct Rectangle{
        float width;
        float height;
    }

Remember, haskell data is immutable, so you won't be able to define functions that change the values in a stored variable. This is why user defined data types are more similar to structs than to objects despite you having full control over what the data looks like.

<br>

---

## Some more features

<br>

Haskell ADTs allow for some pretty interesting typing rules. You can use the `|` operator to define multiple different ways the data type can behave. All are completely independent yet all are equally part of the data type. We can expand our definition of the Rectangle into a broader definition for Shape2D and add triangles and circles to the mix too.

    data Shape2D = Rectangle Float Float |
                   Circle Float |
                   Triangle Float Float Float
                   deriving (Show, Eq)

Side note: the final line of that type definition, `deriving (Show, eq)` tells haskell that you want to be able to test for equality between two Shape2Ds and that you want to be able to print them to the console.

Combining a few different definitions into one data type with the `|` operator is very useful because it lets use something similar to overloaded functions through the use of pattern matching. For example, if you want to calculate the area of a Shape2D, you would only need one expression that makes use of a few different cases.

    area :: Shape2D -> Float
    area (Rectangle n m) = n * m
    area (Circle r) = pi * r * r
    area (Triangle m n p) = (m * n * (sin p)) / 2

When you have multiple values in one data type, it can be tricky to standardize which value means what, especially when all the values are in the same format. One way to remedy this is to use what's called Record Syntax:

    data Shape2D
        = Rectangle
            { length :: Float,
                width :: Float
            }
        | Circle
            { radius :: Float
            }
        | Triangle
            { sideA :: Float,
                sideB :: Float,
                angle :: Float
            }
        deriving (Show, Eq)

This automatically generates the functions that handle the differently named vlaues. Also, if you print out a variable storing a data type defined with record syntax, it will show you which values correspond to which name.

    [Input]
    x = Triangle 3 4 (pi/4)
    x

    [Output]
    Triangle {sideA = 3.0, sideB = 4.0, angle = 0.7853982}

---

## Recursive Data Types

<br>

Perhaps one of the most interesting quirks of Haskell's data type declaration system is the freedom to use recursion directly in the definition of a data type. You may be familiar with the notion of successor notation when working with natural numbers. These are a recursive translation of the counting numbers 0, 1, 2, 3, and so on. Zero would be the base case, and any number greator than zero is the "successor" of the number before it. So, if O represents zero and S represents the successor of a number, then SO would represent 1 and SSSSSSSSO would represent 8. This same idea can be expressed in Haskell with the following data type declaration:

    data NN = O | S NN
        deriving (Eq, Show)

Notice how the name of the data type makes an appearance in the definition of the data type (and how there are 2 "cases" within the definition). In english, that might read "let there be some data type, NN, such that it either equals O or S with a trailing NN". After writing the definition down, load it in GHCi and try out the following commands:

    x = O
    x
    y = S x
    y
    z = S y
    z

Make note of the parentheses that show up when printing the variables.

If you wanted to define an addition function that takes two NNs and returns an NN, it would look something like this. Notice how it is defined recursively!

    addNN :: NN -> NN -> NN
    addNN O n = n
    addNN (S n) m = S (addNN n m)

For more on Haskell's Data Types, check out this fantastic article [here](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).

<br>

[Blog Hub](../index) | [Previous](post2) | [Next](post4)
