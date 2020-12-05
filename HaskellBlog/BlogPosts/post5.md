[Blog Hub](../index) | [Previous](post4) | [Next](post6)

# Post 5<br>Functions Part 2

[Reference](http://learnyouahaskell.com/higher-order-functions#lambdas)
[Reference](http://learn.hfm.io/higher_order.html#sec:ho-flexibility)

## Currying

<br>

I have a confession. There isn't a single function in Haskell that takes more than one parameter. In fact, every single function can take one and only one parameter as input and produce only a single output. But how does that make sense? We've _written_ functions with multiple parameters! Well yes and no...

Yes, it't true that the functions we've written look and act like they can take multiple parameters to produce a single output, but what's actually happening behind the scenes is Haskell creating functions from other functions that are "partially applied". Here's an example:

In GHCI, typing the command `subtract 4 5` will yield the result 1. It is the same as 5 - 4 right? Notice that the output doesn't change if you add some parentheses: `(subtract 4) 5` still yields 1. In the `(subtract 4)` portion of that command, Haskell creates a new function that will subtract 4 from anything passed to it. The value 5 happens to be passed to that new function, and we're left with our regular result. You can confirm this by trying out this sequence of commands:

    x = subtract 4
    x 5

Once again, you're left with the value 1.

The action of applying parameters to partially applied functions is called Currying. A more formal definition is as follows: "the technique of translating the evaluation of a function that takes multiple arguments (or a tuple of arguments) into evaluating a sequence of functions, each with a single argument." [source](https://hackernoon.com/currying-in-the-real-world-b9627d74a554)

This weird property is also why function declarations behave like they do! The operator `->` let's Haskell know two things about functions: what the type of the parameter is (on the left of the arrow), and what the type the function returns (and on the right of the arrow). The function

    addFourNum :: (Num a) => a -> a -> a -> a -> a
    addFourNum x y z w = x + y + z + w

Doesn't actually four parameters, but rather takes 1 parameter and returns a function of type `(a -> a -> a -> a)`, which then takes 1 parameter and returns a function of type `(a -> a -> a)`, which then takes 1 parameter and returns a function of type `(a -> a)` which finally returns a value of type `a`

    fivePlusThreeNum :: (Num a) => a -> a -> a -> a
    fivePlusThreeNum = addFourNum 5

    eightPlus2Num :: (Num a) => a -> a -> a
    eightPlus2Num = fivePlusThreeNum 3

    add12 :: (Num a) => a -> a
    add12 = eightPlus2Num 4

At the end of all that, calling `add12 6` would leave you with the value 18. That is essentially the same as simply calling `addFourNum 5 3 4 6`

This might not seem super useful right off the bat, but it allows for very flexible use of preexisting code in ways that are impossible without currying. Say, for example, you have a complicated function with tons of parameters. If you found yourself frequently setting just one of those parameters to one value, you could easily define a brand new function that is essentially the same, but with that parameter already set!

<br>

---

## Higher Order Functions

<br>

We've seen that functions can return functions as outputs, but what about functions that recieve other functions as inputs? This is possible too!

We now have a better understanding of the arrow operator when declaring functions, so let's think about what happens when we add some parentheses.
The function heading `func :: a -> a -> a` means exactly the same things as `func :: a -> (a -> a)` because func, at its core, returns a function that then takes one input and produces an output of the same type. In that way, the arrow operator is "right associative". Well, what if we put the parentheses in a different spot? The heading `func :: (a -> a) -> a` is now a function that takes a function as input and produces an output. Huh... that's a weird one isn't it!

To see this in action, here's an example from the wonderful Haskell Tutorial, [Learn You a Haskell](http://learnyouahaskell.com/higher-order-functions). It is a function that accepts another function and applys it twice to whatever parameter you give it.

    applyTwice :: (a -> a) -> a -> a
    applyTwice f x = f (f x)

If you curry it with the add12 function from earlier watch what happens! Type the following in GHCI after loading all the functions from this blog post

    x = applyTwice add12
    x 4

That should print out 28 to the console!

<br>

---

## Applications of Higher Order Functions

<br>

There are tons and tons of applications for Higher Order Functions. Some of the most common are maps, zippers, and filters, all of which have to do with list manipulations. Let's look at an example of each!

Maps are Higher Order Functions that take a function (that itself takes one parameter and returns a single value) and a list as parameters and return another list. The type contained in the list parameter has to match the type of the input of the passed function, the output of the passed function can be a different type. Maps apply the passed function to every element of the list and returns the transformed (or mapped) list. The following function is already defined in the prelude of Haskell:

    map :: (a -> b) -> [a] -> [b]
    map f (x : xs) = f x : map f xs

Calling something like `map (^2) [2,3,4,5]` should return the list `[4,9,16,25]`. Every element of the list is squared. (Note: the expression `(^2)` is a partially applied function, shorthand for `x ^ 2`. It is possible to have the same syntax for multiplication and addition. It is also possible to change the order, so calling map with `(2^)` would return a list where each element is two to the power of the elements of the input list)

Zippers are similar to maps, but instead of only modifying one list, they are a way to combine two separate lists into one by some rule defined by the input function. Once again, the following function already exists in Haskell:

    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith op []       _        = []
    zipWith op _        []       = []
    zipWith op (x : xs) (y : ys) = (x `op` y) : zipWith op xs ys

Notice that the types of the input function can be any three types or all three the same type. Likewise, the types of the lists have to match the types of the function. Try the following command and see what happens!

    zipWith gcd [543, 424, 166] [843, 594, 735]

You'll get a list of the elementwise greatest common divisors between the two lists: `[3,2,1]`

Lastly, filters are functions that filter out data from lists given some boolean function:

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter f (x : xs) = if f x then x : filter f xs else filter f xs

If you try out the command `filter (>5) [2, 6, 7, 3, 1]` you should be left with the list `[6, 7]`, as those are the only two numbers greater than 5 in the list

See the references for more applications of Higher Order Functions!

<br>

---

## Quick note: Anonymous functions

<br>

In some cases, you won't need to pass a fully defined function to a higher order function, rather you just need a one time use expression. To do this without having to fully implement a function that won't be used more than once, just use anonymous functions! These come directly from [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus), and the syntax reflects that. Remember that the **sole** purpose of anonymous functions is to be passed to higher order functions as parameters.

Anonymous functions come in the form:

    (\x -> [Some Expression])

The `x` is treated as a parameter and can be used in the expression. Values can be passed to the expression like any other function.

Let's take a look at a simple example. Try this out in GHCI

    (\x -> x + 5) 10
    --> 15

You can also use multiple parameters, and they can have any valid identifier as a name, as long as that name isn't the name of another function.

    (\first second third -> first * second + third) 2 5 6
    --> 16

[Blog Hub](../index) | [Previous](post4) | [Next](post6)
