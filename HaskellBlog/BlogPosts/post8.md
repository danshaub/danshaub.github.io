[Blog Hub](../index) | [Previous](post8) | [Next](post9)

# Post 8<br>Input/Output and Compiling

## Compiling Haskell Programs

<br>

So far, we've only run our Haskell programs in GHCI. That's been fine for demonstration purposes, but what if you actually want to distribute your program? The answer is... **_GHC_**, the non-I version of GHCI! It's not really that exciting, it's just compiling your haskell program into an executable.

This is done simply with the `ghc` command! Go ahead and try to compilie some of your code by running the following command in your terminal (or powershell if you're using windows):

    ghc --make [file path]

The only problem with that is if we tried to compile any of the code we've written so far, it would just spit out an error:

    The IO action `main' is not defined in module `Main'

That's a pretty good clue into why our code isn't complining! Thank you GHC! We are missing a `main` function, _just_ like is necessary in so many other languages.

The astute among you may notice that the error message makes reference to `main` being an "IO action". You may also recall that I've mentioned IO to be a type of monad. Good eye!

As it turns out, the function header for `main` is `main :: IO ()`

It should seem obvious that `IO ()` would be the type of `main`. For the program to do _anything_ useful, there has to be some way for the user and the program to communicate, and input/output is the way to do that. So, main is a function that take no arguments and returns an `IO ()` monad and is the entry point into our program.

If you're working on a project larger than one file, it is useful to add the line

    module Main where

at the top of your code to denote the file containing the `main` function as the where the computer should look for the entry point into the program.

---

## Simple Commandline Output

<br>

Well then, how do we actually use the `IO` monad? Very similarly to how we've used other types of monads: with the `do` notation!

So, let's our journey with the timeless "Hello World" program! (Yes, it's coming 8 posts in, but at least you'll understand it better than if we had done it at the start!)

    module Main where

    main :: IO ()
    main = do
        putStr "Hello World\n"

Compile your program and execute it:

    [Windows]
    ghc --make .\HelloWorld.hs
    .\HelloWorld.exe

    [Unix]
    ghc --make ./HelloWorld.hs
    ./HelloWorld

BOOM! Now you're as proficient in Haskell as you were in your first language on day one of learning how to code! Except for, well, all the other stuff you already know about Haskell.

You may have guessed this already, but the `putStr` function is of type `String -> IO ()`, so in the example we have above, you don't actually need the "`do`". However, if you want more than a single line of output (among other things) you need to use `do` or the monad notation using `>>=` and `return`, but that's yucky so we'll stick with `do`.

Something else to note, when using multiple `putStr` statements, you have the _option_ of using the left facing arrow for any line but the final, however it isn't necessary. The following two programs are equivalent:

    main = do
        putStr "Line 1\n"
        putStr "Line 2\n"

    main = do
        x <- putStr "Line 1\n"
        putStr "Line 2\n"

In the second program, the `x` wouldn't really have a useable value associated with it, so skipping the `_ <-` is fine. And remember, the final line of the do statement can't have a `<-` because the result of whatever expression is on the last line is what the function (`main`, in this case) as a whole evaluates to.

Just like many other languages, Haskell also has a function that automatically adds a new line character to the end of a line it prints. This is simply `putStrLn`. It also has `putChar` that acts how you would expect.

Say you wanted to print a list or just a number, or really anything that isn't already a string. Well, as long as it derives from `Show` then just use the `print` function!

     main = do
        print [1, 2, 3, 4]

---

## Simple Commandline Input

<br>

What we have now is fantastic, but it doesn't quite let the user control the program just yet. For that we'll need some input functions! After all, IO doesn't stand for ioutput, it stants for **_input_** output!

    main :: IO ()
    main = do
        putStrLn "What is your name?  "
        x <- getLine
        putStrLn ("Hello " ++ x ++ ", how are you?")

The outcome of this code should be pretty self explanatory. Clearly, the `getLine` function asks for a line of input and sets the outcome of the `getLine` function to be the value of `x`.

As you might expect, a `getChar` function exists too!

---

## File IO

<br>

You can also, fairly easily, read from and write to files using the `IO` monad. To handle file names and paths, the type `FilePath` is used. Luckily, it's just a wrapper for `String`, no conversion necessary.

To write to a file, use the function `writeFile :: FilePath -> String -> IO ()`:

    [Windows]
    main = do
        putStrLn "What is your name?  "
        x <- getLine
        writeFile ".\\temp.txt" ("Hello " ++ x ++ ", how are you?")

    [Unix]
    main = do
        putStrLn "What is your name?  "
        x <- getLine
        writeFile "./temp.txt" ("Hello " ++ x ++ ", how are you?")

To read from a file, use the function `readFile :: FilePath -> IO String`:

    main = do
        x <- readFile ".\\temp.txt"
        putStrLn x

Notice that the entire contents of the file are stored in `x`. You can do with that string whatever you please!

For the full list of IO based functions defined in `prelude` see [here](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#g:26)

---

[Reference](http://learnyouahaskell.com/input-and-output)

[Blog Hub](../index) | [Previous](post8) | [Next](post9)
