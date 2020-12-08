[Blog Hub](../index) | [Previous](post5) | [Next](post7)

# Post 6<br>Monads Part 1

[Reference](https://www.cs.rit.edu/~swm/cs561/All_About_Monads.pdf)
[Reference](https://www.youtube.com/watch?v=t1e8gqXLbsU&ab_channel=Computerphile)

## The "Maybe" Type

<br>

Before we begin a broader discussion of monads (which will only be an introduction, all things said and done), it's best to begin with a relatively simple to understand example of a monad: the "Maybe" type constructor:

    data Maybe a = Nothing | Just a

This is essentially saying that something of type `Maybe a`, could either be nothing at all, or it could just be some value of `a`. For example

    netWorth1 = Just $250,000
    netWorth2 = Nothing

could represent a bit of data pulled from a set that might be incomplete. For one data point, there could be a value for netWorth, but for another, there could be no value present.

Some functions built in to Haskell return a "Maybe" type. Look back at [post 4](post4) where we used the function `T.findIndex` which has type `(Char -> Bool) -> Text -> Maybe Int`. It takes a function from `Char` to `Bool` and a `Text` object and returns maybe an integer. If it doesn't find a character within the string that makes the boolean function evaluate to true, the whole function returns nothing.

<br>

---

## Monads in Haskell

<br>

In haskell, monads are simply a type class that contains definitions of two functions: `>>=` and `return`. Specifically, the class is implemented as follows:

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a

As we saw with the `Maybe` monad type, they can be thought of as "containers" with special properties. The `Maybe` monad offers a way to store a number, string, or some other kind of data type in a special box that could also contain nothing. The two functions defined in the `Monad` class offer a way to modify the data within the container (`>>=`, a.k.a "`bind`") and a way to wrap some data in the container (`return`).

Implementation of the `Maybe` monad would look as follows (note the use of currying in the definition of `return` and the use of higher order functions in the definition of `bind`!)

    instance Monad Maybe where
        Nothing >>= f = Nothing
        (Just x) >>= f = f x
        return = Just

Let's see another simple example of how monads might be used. This function, `maybeAdd` accepts two `Maybe Int`'s as parameters and returns a `Maybe Int`.

    maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
    maybeAdd mx my = mx >>= (\x -> my >>= (\y -> return (x + y)))

This is usually written in the following format to reflect a type of notation we'll talk about later.

    maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
    maybeAdd mx my =
        mx >>= (\x ->
        my >>= (\y ->
            return (x + y)))

Let's break this down a bit. The use of return is the easy part: simply add `x` and `y` together and wrap that in a `Maybe`. The rest of the line is a bit more complicated but is vitally important in how this function works. That part accounts for one or both of the inputs being `Nothing` and makes sure the function evaluates to `Nothing` in that case. Recall that the type of `>>=` is `m a -> (a -> m b) -> m b`. What we're seeing is a bit of function composition, so let's start with the inside call of `>>=`: `my >>= (\y -> return (x + y))`. Here we see an anonymous function that takes some parameter, y and evaluates to `return (x + y)`. It is of type `a -> m b` as required by `>>=`. That function is curried and given as the anonymous function with parameter `x`, notice the `(\x -> `.

The syntax of that is a bit clunky, so luckily the developers of Haskell introduced `do` notation. This has absolutely nothing to do with `do-while` loops of other languages. The `do` notation is simply shorthand for repeated uses of `bind` and `return`. Anything that can be done with `do` can be done longhand as well, it just looks nicer and is a bit more readable. Translated into `do` notation, our `maybeAdd` function looks like:

    maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
    maybeAdd mx my = do
        x <- mx
        y <- my
        Just (x + y)

The special left pointing arrow, `<-` lets you easily translate from a monad to the value it stores (or the evaluation of some function on the value it stores), and use it in a final expression that evaluates to the type of monad you're working with (as seen in the line `Just (x+y)`).

<br>

---

## What's the point?

The main reason monads are used is to add "side effects" into functional programs. This can be thought of as injecting little pockets of imperative-ness into a functional program. They are necessary for writing programs that change based on certain circumstances, while still remaining functional.

As we'll see in the next post, Monads pop up everywhere in Haskell. Lists, I/O, error handling, and program states.

[Blog Hub](../index) | [Previous](post5) | [Next](post7)
