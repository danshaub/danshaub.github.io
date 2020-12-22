[Blog Hub](../index) | [Previous](post8) | [Next](post10)

# Post 9<br>Functional Programming in F♯

## What is F#?

<br>

F# (pronounced F-sharp) is Microsoft's take on functional programming! Transitioning from Haskell to F# should be just like transitioning from one imperitive langauge to another, so let's have some fun exploring the syntax of a brand new language!

For more on F#, definitely check out Microsoft's own [documentation on the language](https://docs.microsoft.com/en-us/dotnet/fsharp/what-is-fsharp) (it's better written than this could ever hope to be, but come back anyway, I'm more fun than them)

---

## Installing F#

<br>

Again, [Microsoft has you covered](https://docs.microsoft.com/en-us/dotnet/fsharp/get-started/install-fsharp) with installation steps. Luckily, if you've been using Visual Studio Code (as you should be), F# is a box-standard language! Just make sure to install the extension Ionide-fsharp. It's a standard extension that gives language support, very useful.

---

## Let's Get Rolling!

<br>

Just like we did with Haskell, let's start with the interactive terminal; F#'s equivalent to ghci. To open it, simply run the command `dotnet fsi` from the console/power shell. The first difference you'll notice is fsi's insistance on terminating every single command with `;;`. It's a bit irritating at first, but you'll see that it's useful for declaring multi-line functions directly in the terminal.

Let's start with the box-standard commands:

    > let x = 10;;
    > x;;

Take a wild guess at what this does..... it assigns the integer value `10` to the name `x`! How surprising! The `let` keyword is all about declarations

Lists? Of course!

    > let xs = [1,2,3];;

Hmmmmm.... that was unexpected. Fsi interpreted that command as a single element list of type (int _ int _ int), or a tuplet of three integers. As it turns out, the list delimiter is a semicolon, not a comma.

    > let xs = [1;2;3];;

Ahhhhh that's more like it! You can also declare lists on multiple lines with the following syntax: (be sure to observe indentation!)

    > let xs = [
    -     1
    -     3
    -     5
    - ];;

The head-tail syntax from Haskell (`x:xs`) is similar in F#, just with two colons instead of one:

    > -1 :: xs;;

    output: val it : int list = [-1; 1; 3; 5]

And, if you're a data structures expert, arrays are declared in a very similar way, but with some added pipes `'|'`

    > let xa = [|1; 3; 5|];;

What's really cool about arrays in F# is that they're **mutable**! Meaning, you can modify individual elements without copying the entire array. They're also faster to index because they have a fixed size, but you knew that already :) The following syntax shows how to index an array and modify an element of it:

    > xa.[1] <- 33;;
    > xa

    output: val it : int [] = [|1; 33; 5|]

---

## Declaring Functions

<br>

As I mentioned earlier, you can very easily declare functions directly in fsi. This, once again, uses the `let` keyword!

    > let square x = x * x;;
    > square 5;;

    output: val it : int = 25

This also works for multi-line functions.

    > let isEven x =
    -     if (x % 2) = 0 then
    -         true
    -     else
    -         false
    - ;;
    > isEven 4;;

    output: val it : bool = true

---

## Writing Files

<br>

F# actually has a few different file extensions, but the one to use when writing programs meant to be loaded in fsi is .fsx

Let's make the "functional programming hello world" (aka fibonacci). make a brand new file named `fib.fsx` and type in the following:

    let rec fib x =
        if x = 0 then 0
        else if x = 1 then 1
        else (fib (x - 1)) + (fib (x - 2))

Save it, then enter fsi from the same directory as where you saved `fib.fsx`. You can run the function you wrote like this:

    > #load "fib.fsx";;
    > Fib.fib 5;;

    output: val it : int = 5

And there you have it! A short intro to F#

[Blog Hub](../index) | [Previous](post8) | [Next](post10)
