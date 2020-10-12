[Blog Hub](../index) | [Previous](week1) | [Next](week3)

# Week 2<br>Defining functions in Haskell

## The .hs File extension

Haskell source code files have the extension .hs. These files can be loaded in GHCi and compiled into programs with GHC. Let's explore a little of what you can do with haskell files.

<br>

---

## Function Definitions and Recursion

Writing functions in Haskell is very similar to writing pure mathematical definitions. The interpreter is able to parse your definitions and allocate memory automatically, so you don't need to worry about memory management or anything like that. Functions often take the form of recursive definitions with a a base case or a few, and a recursive case.

Take, for example the factorial function. As you may know, n! = 1 _ 2 _ ... _ n-1 _ n for any integer n. Also, 0! evaluates to 1. Using those two facts, let's write a function `fact` that takes a single integer as an input and returns an integer as an output.

    fact 0 = 1
    fact x = x * fact (x - 1)

Save your file and enter GHCi from within the same folder as your new haskell file. To load your new expresion into GHCi, you use the `:load [file name]` command. With your program loaded, try a few expressions in GHCi:

    fact 0
    fact 4
    fact 12334
    fact -1

The Haskell interpreter automatically assigns types to the parameters in the expression. Since Haskell "knows" that passing a negative integer would cause the function to run forever, it completely disallows it from happening.

Some text editors (I would recommend visual studio code) will show you what the implicit typing of your function is and will give you the option to declare it on the spot.

Haskell functions rely on pattern matching to ensure the correct case is chosen for any given value passed to the function. The order in which you give the patterns is very important because the interpreter will attempt to match patterns with the given parameters sequentially.

<br>

---

## Some more examples and some more syntax

Here is a function that calculates the length of a list recursively

    len [] = 0
    len (x : xs) = 1 + len xs

Here are some functions that split a list into it's odd and even number indicies:

    select_evens :: [a] -> [a]
    select_evens [] = []
    select_evens [x] = []
    select_evens (x : y : ys) = y : select_evens (ys)

    select_odds [] = []
    select_odds [x] = [x]
    select_odds (x : y : ys) = x : select_odds (ys)

When using the : operator, as long as the right side evaluates to a list, you're good to go! Because of that, Haskell will evaluate the right side first. That's how (x : y : xs) works!

If you need to have some control flow, try the "if then else" structure! When used in a recursive function, the function only recurses if the condition you give it is met So this function checks if the current element of a list is equal to the key, m. Otherwise it recurses on the next element of the list

    member m [] = False
    member m (x : xs) =
        if m == x
            then True
            else member m xs

You can also use haskell's lazy boolean operators: The boolean operator || means "or", however the right side of the operation isn't evaluated if the left side is true Likewise, when evaluating &&, if the left side is false then the right side isn't evaluated

    member2 [] y = False
    member2 (x : xs) y = (x == y) || (member2 xs y)

Here's how to reverse the order of a list:

    revert [] = []
    revert (x : xs) = append (revert (xs)) (x : [])

You can nest conditional statements too!

    less_equal ([], []) = True
    less_equal (x : xs, y : ys) =
        if len xs /= len ys
            then False
            else
                if x <= y
                    then less_equal (xs, ys)
                    else False

As part of the larger merge_sort algorithm, the merge algorithm takes two sorted lists and combines them into another sorted list. It does this by checking the first element of each list and adding the smaller of the two to a new list. It then moves on to the next element of the list that contained the smaller value, and recurses.

    merge ([], []) = []
    merge (x : xs, []) = x : xs
    merge ([], y : ys) = y : ys
    merge (x : xs, y : ys) =
        if (x <= y)
            then x : merge (xs, y : ys)
            else y : merge (x : xs, ys)

The actual merge_sort funcion simply splits an unsorted list in half, then recurses. Once all the lists have a length of 1 or 0, the lists are merged in pairs until all the smaller lists are merged into one sorted list.

    merge_sort [] = []
    merge_sort [x] = [x]
    merge_sort xs = merge (merge_sort ys, merge_sort zs)
        where
            n = len (xs) `div` 2
            (ys, zs) = splitAt n xs

The "where" operator lets you define values to be used in an expression.
In this case, n is the index at which larger lists are split into smaller ones,
ys is the first half of the larger list, and zs is the second half.

<br>

---

[Blog Hub](../index) | [Previous](week1) | [Next](week3)
