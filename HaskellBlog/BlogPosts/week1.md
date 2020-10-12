[Blog Hub](../index) | [Next](week2)

# Week 1:<br>Introduction to Functional Programming and Haskell

## Functional Programming

<br>

Functional programming is a programming paradigm that stands opposite Imperitive Programming. Whereas imperitive programming is all about changing the state of variables and objects stored in memory, functional programming is all about mathematical, deterministic, functions. Functional programming languages are used in many places in industry from Facebook's spam filter to AT&T's automated internet abuse complaint processing.

One of the biggest advantages functional programs have over imperative programs is functions without side effects. What that means is that functions in languages like Haskell are unable to modify variables sent as parameters. Every function is pass by value and returns a deterministic result. This allows functions that are mathematically equivilant to be used interchangably without fear of differing side effects between functions. For example two list sorting functions might differ in implementation and therefore have different time complexities, however the result will be the the exact same, so you are always able to use the fastest sorting algorithm.

Another advantage of functional programs is their ability to run functions in parallel without any of the side effects that would come from sharing memory between CPU threads. This is because functions are unable to modify any data passed to them; remember, functions can only take values as parameters, not references to memory locations. Highly parallelized programs are incredibly efficient, especially when run on modern processors with many CPU cores.

Functional programming languages are relatively easily used when prototyping and developing applications. The declarative and deterministic nature of functional programming makes for fewer bugs and more accurate and efficient program design. There are also many facilities for rapid and automated unit testing that speed up debugging even more.

<br>

---

## Installing Haskell

<br>

Haskell is very easy to install and use. In fact, the installation page says haskell is "designed to get you up and running quickly". If you're using windows, it can be useful to install haskell both on your main OS for use in Power Shell, and in a Linux virtual machine of your choosing.

See the haskell installation page [here](https://www.haskell.org/platform/)

<br>

---

## First steps with Haskell

<br>

Once you have haskell installed, go to your command line and use the command `ghci` to open up your haskell environment. "GHC" is the Glasgow Haskell Compiler and "i" stands for interactive. Later, we'll see how to use GHC proper.

GHCi lets you run any and all loaded haskell functions, assign values to variables, load new functions, and execute various commands.

One command is `:quit` wich lets you exit GHCi and return to your command line. Ctrl+D does the same thing.

By default, the Prelude and Standard Packages are loaded in GHCi at statup. Included in those packages are many expressions and data types. Let's play with them a bit.

Try the following commands and see what happens:

    1 + 2
    3 * 4
    7 - 2
    (12 * 3) / 4
    (12 * 3) `div` 4

---

## What's with the backtick??

When calling a function (or writing a function, as you'll see later), Haskell often makes use of prefix notation without parentheeses. so something like `gcd(4,9)` would become `gcd 4 9` in Haskell. (This is a little bit tricky to get used to, but it gets easier the more you use it!)

The backticks from the division example above are used to "translate" from prefix notation to infix notation, so <code>gcd 4 9</code> becomes <code>4 `gcd` 9</code>. Try the following:

    div 45 9
    45 `div` 9
    22.5 `div` 4.5

Something to note: haskell expressions are not polymorphic. There can only ever be one set of strongly typed parameters for each function name. Haskell's div function ONLY works for integers while the division slash works for floating point numbers.

<br>

---

## Setting some variable names

As mentioned before, haskell lets you give values to variable names. These don't, however, work quite the same way as variables in imperative languages. Variables are immutable and can ONLY pass their value, not their reference. They can be overwritten, but they may not be modified.

    x = 5
    6 * x
    y = x * 3
    print y
    y = x * 4
    print y
    x = x * 5 + y
    print x

You may have noticed GHCi stall for a while. Press Ctrl + C to interrupt the process. Haskell allows for recursive definitions even if they might not terminate. So, when haskell tried to overwright the value for x with a definition including x, it does so recursively, looking to the definition of x from the same line to define x. Because there is no "base case" for this recursion, the process repeats infinitely.

<br>

---

## Lists

**_Add some examples of how to work with lists_**

[Blog Hub](../index) | [Next](week2)
