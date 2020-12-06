[Blog Hub](../index) | [Previous](post6) | [Next](post8)

# Post 7<br>Monads Part 2

[Reference](https://www.cs.rit.edu/~swm/cs561/All_About_Monads.pdf)
[Reference](https://www.youtube.com/watch?v=t1e8gqXLbsU&ab_channel=Computerphile)

## The Monad Axioms

<br>

Monads come from a field of mathematics called "[Category Theory](<https://en.wikipedia.org/wiki/Monad_(category_theory)>)" [WARNING: There's a lot that go into definitng monads in category theory. Clicking on that link and trying to understand it is a high level math rabbit hole. Luckily, most of that math is unnecessary for understanding how the monad axioms work in Haskell]. In category theory, there are a series of three laws or axioms that define a monad, one for "Left identity", one for "right identity" and one for assiciativity, all of which standardize how `return` and `>>=` relate to one another.

The first axiom, Left Identity can be expressed in haskell as follows: (the use of `≡` denotes equivalency. In other words, the expression on the left can be replaced with the expression on the right and the functionality of the program won't change.)

    return x >>= f ≡ f x

Recall that the type of `f` must be `a -> m b`, the type of `return` is `a -> m a`, and the type of `>>=` is `m a -> (a -> m b) -> m b`. With that in mind, this axiom is saying that binding the returned value of `x` to `f` is the same as simply evaluating `f x`.

---

The second axiom, Right Identity can be expressed as follows:

    m >>= return ≡ m

Again, when reasoning about the typing of every component of the statement, it's clear that this is stating binding a monad type to its return function is the same as just having the monad. This is because bind applys a function that takes a pure data type and returns a monad to the value stored in a monad. So if that function simply returns that value wrapped in a monad, you're back to where you started.

---

The third axiom, Associativity, is a bit trickier to grasp than the first two:

    (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)

or:

    (m  >>=  (\x -> f x))  >>=  g    ≡
     m  >>=  (\x -> f x    >>=  g)

What on earth is this saying??? Well, this expression is in a similar form to our maybeAdd function from [last post](post6). It's simply a statement about composition of bound functions. Namely if you bind a monad to a function and then bind the result of that binding to another function, it is the same as composing the bound functions into one and then binding that composition to a monad. This is what allowed us to add the values stored in two separate monads together, and what allowed us to do it with the use of only one "`do`"

Perhaps a more clear way to see this axiom in action is to put it in terms of `do` notation. The following three statements are all equivalent:

    do
        y <- do
            x <- m
            f x
        g y

    ≡

    do
        x <- m
        do
            y <- f x
            g y

    ≡

    do
        x <- m
        y <- f x
        g y

For more on the monad axioms, check out this [wiki post](https://wiki.haskell.org/Monad_laws)

---

## Lists as Monads

<br>

As mentioned in the previous post, lists in Haskell are technically monads (althought that is often irrelevant when using them in a lot of applications)

Lists are defined as follows:

    instance Monad [] where
        m >>= f = concatMap f m
        return x = [x]
        fail s = []

Lists in Haskell must be treated as monads because of one fundamentally non-deterministic aspect of them: thier length. Haskell doesn't necessarily hold on to the length of a list the way that imperative languages do, so when programs evaluate functions with lists, there's no way to know when the function will end or how many results will be returned. This is similar to how the `Maybe` monad will either evaluate to one result or zero results; lists can evaluate to one, zero, or multiple results in a single function call!

[Blog Hub](../index) | [Previous](post6) | [Next](post8)
