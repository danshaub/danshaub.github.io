[Blog Hub](../index) | [Previous](post3) | [Next](post5)

# Post 4<br>Roman Numeral Calculator (Project)

## Review of Roman Numerals

<br>

Roman numerals are fairly common in the modern world despite the more sophisticated numerals we use in math today. They are built from a series of simple rules that give strings of characters made up from "I", "V", "X", "L", "C", "D", and "M", numerical meaning. In order the characters represent the values 1, 5, 10, 50, 100, 500, and 1000. It would be very easy (albeit very messy) to just throw the letters together in any order and simply add the values of each character to get the value of the string as a whole. However, keeping things in order and allowing for special cases increases readability of the numerals immensely. Those added cases are useful when working with pen and paper, but as we'll see, they can be quite the pain when dealing with roman numerals on a computer. (For more on Roman Numerals, see [here](https://www.mathsisfun.com/roman-numerals.html))

<br>

---

## String Rewrite Systems

<br>

When writing a calculator in Haskell that handles Roman Numerals, it is important to be aware of the logic behind String Rewrite Systems.

In principle, they are a series of rules that allow incremental modifications to strings of characters. A very simple rewrite system would be the following:

-   Let **n** and **m** be strings consisting only of characters `[a,b]`
    -   **n** `ab` m -> **n** `ba` **m**
    -   **n** `ba` m -> **n** `ab` **m**
    -   **n** `aa` m -> **n** `b` **m**
    -   **n** `bb` m -> **n** `a` **m**

###### Note that the rules listed are only rules that _can_ be applied to the string, not rules that _must_ be applied.

In this example the string "abaabba" could be rewritten as "abbbba" then as "aabba" then "aabab" and so on. The string is modified according to the rules of the rewrite system.

If you allow rules to be applied in reverse (i.e. **n** `b` m -> **n** `aa` **m**), then the strings that can be written by applying the rewrite rules an arbitrary number of times are said to be in the same _equivalency class_. There can be any number of equivalency classes in a rewrite system. If it is impossible to transform one string into another only by applying the rules of the system, then those two strings aren't in the same equivalency class.

As an exercise, think about the strings "baba", "bab", and "a". Are they all in the same equivalency class? Are they each in different ones? Can you come up with equivalency classes for the system that aren't represented by the three strings? Before scrolling down, take a minute to ponder that.

It turns out, that each one is in a separate class. There is no way to apply the rules (forward and backward) that will let you transform one string into one of the others. It also turns out that there are only three equivalency classes for this system.

### Invariants with Equivalency Classes

You may be wondering: is it possible to know if two words are in the same equivalency class without explicetly checking by applying the rules? If it is, then how?

The answer is yes, using invariants!

Invariants are properties of the strings that remain constant after any of the rules are applied. If two strings have the same value for their invariant, then they're in the same equivalency class!

This property could be anything from the length of the string, to the number of a's and b's, to a weighted sum of the characters, etc. As long as the property doesn't change when ANY of the rules are applied, it's a valid invariant.

For the example above, it may be tempting to look at the first two rules and say the invariant is that the number of a's and b's doesn't change. You would be correct in saying stating that as an invariant for those two rules only, however that doesn't hold when applying the second two rules. In this case, the value that doesn't change regardless of the rules applied is

-   ( (num a) - (num b) )%3

In other words, if you find the difference in the number of a's and b's then calculate modulo 3 of that difference, it will be the same if you apply any of the rules to the string and recalculate. Let's verify with the strings we used above:

-   "baba" -> "abab"
    -   "baba": (num a) = 2, (num b) = 2
    -   "baba": (2 - 2) % 3 = 0
    -   "abab": (num a) = 2, (num b) = 2
    -   "abab": (2 - 2) % 3 = 0
-   "bab" -> "aa"
    -   "bab": (num a) = 1, (num b) = 2
    -   "bab": (1 - 2) % 3 = 2
    -   "aa": (num a) = 2, (num b) = 0
    -   "aa": (2 - 0) % 3 = 2
-   "a" -> "bb"
    -   "a": (num a) = 1, (num b) = 0
    -   "a": (1 - 0) % 3 = 1
    -   "bb": (num a) = 0, (num b) = 2
    -   "bb": (0 - 2) % 3 = 1

It is sufficient to show that the invariant applys to each rule individually to prove that the invariant applys to the system as a whole.

<br>

---

## String Rewrite Systems in Haskell

<br>

String rewrite systems on their own don't compute anything, they simply are a series of rules that _could_ compute something. To turn that "_could_" into a "_does_" all you need is to give a precidence order to the rules, and to tell a computer to apply the rules until it can't anymore.

You may notice that a computer could get stuck applying the first two rules of our example system until the heat death of the universe and still never finish. To remedy that, simply remove one of the rules that switches the order. As long as one of them remain, our equivalency classes won't change.

When a string exists within an equivalency class to which none of the rules can be applied, that is said to be the **normal form** of the equivalency class. Usually, when implementing string rewrite systems in code, the goal is to compute the normal form of some string, which is exactly what we'll do later with Roman Numerals.

In Haskell, one method to implement the rules of rewrite systems as single pattern functions with a series of cases written with "guard notation". Guards function very similar to switch statements and conditional statements.

The following example of how to use guards is taken from [Learn You a Haskell](http://learnyouahaskell.com/syntax-in-functions), a fantastic online Haskell tutorial. This simple example analyzes your BMI. As you can see, before the `=` on each line, there is a boolean statement. On the final line, the keyword `otherwise` is the equivalent to `else` or `default`. You can have as many cases as you like when using guards.

    bmiTell :: (RealFloat a) => a -> String
    bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're weight is normal"
    | bmi <= 30.0 = "You're overweight"
    | otherwise = "You're a obese"

If you wanted to compute some value to be used in the equivalence statement, you can use the `where` keyword if the value will be calculated the same for every case, or the `let` keyword if there's a different calculation for each case (We'll see the use of `let` later).

    bmiTell :: (RealFloat a) => a -> a -> String
    bmiTell weight height
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're weight is normal"
    | bmi <= 30.0 = "You're overweight"
    | otherwise = "You're a obese"
    where
        bmi = weight / height ^ 2

Haskell's strings are relatively lacking in features which would make implementing the rewrite system significantly harder, so instead we're using the Data.Text package. Make sure to have

    import qualified Data.Text as T

at the top of your program so you can use the package. From the package we're mostly going to use `T.isPrefixOf`, `T.isInfixOf`, and `T.isSuffixOf`, as well as the various methods that combine strings. For full documentation of the functions and data types, see [here](https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html)

So, an implementation of our string reqrite system could be split across a few functions that all work together:

    -- This function applies the rule "ba" -> "ab" as many times as possible, leaving a string with all a's on the left and b's on the right
    -- It also dropps any characters that are invalid in the rewrite system
    order :: T.Text -> T.Text
    order x
    | let ind = M.fromJust (T.findIndex ('b' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'b') x = T.snoc (order (T.append bef (T.tail aft))) 'b'
    | let ind = M.fromJust (T.findIndex ('a' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'a') x = T.snoc (order (T.append bef (T.tail aft))) 'a'
    | otherwise = T.empty

    -- This function applies the rules to reduce two of one letter into one of the other letter, it also makes sure the word stays ordered after
    applyRules :: T.Text -> T.Text
    applyRules x
    | T.isPreixOf (T.pack "aa") x = applyRules (order (T.append (T.drop 2 x) (T.pack "b"))
    | T.isSuffixOf (T.pack "bb") x = applyRules (order (T.append (T.pack "a")(T.dropEnd 2 x) )
    | otherwise = T.empty

    -- This function begins the recursion of the other two functions
    normalize :: T.Text -> T.Text
    normalize x = applyRules (order x)

Not quite as simple as just writing out the system! Let's unpack what's going syntactically starting with the order function.

Both of the behemoth lines in the order function both do the same thing but for b's and a's respectively: They first search for the letter in the string, then they calculate the index of where the letter appears, then finally concatenates the string before that point to the string after that point, recurses on that concatenated string, then tacks on the removed letter at the very end. The reason this is applied to both b's and a's is to ensure that all other letters are dropped (see the final `otherwise` line). If you did this on pen and paper, calling the order function on a string would look something like this:

    order "baba"
    (order ("" ++ "aba")) ++ "b"
    (order "aba") ++ "b"
    (order ("a" ++ "a")) ++ "b" ++ "b"
    (order "aa") ++ "b" ++ "b"
    (order ("" ++ "a")) ++ "a" ++ "b" ++ "b"
    (order "a") ++ "a" ++ "b" ++ "b"
    (order ("" ++ "")) ++ "a" ++ "a" ++ "b" ++ "b"
    (order "") ++ "a" ++ "a" ++ "b" ++ "b"
    "a" ++ "a" ++ "b" ++ "b"
    "aabb"

Note that since the first line checks for b's, all b's will be moved over before any of the a's are touched. Then all the a's are moved before the b's. Finally all other characters, if there are any, are dropped. But how does it do that?

The first part of the line, just after the guard, `let ind = M.fromJust (T.findIndex ('b' ==) x)`, defines `ind` to be the index of the first orrucance of the letter in question. The function M.fromJust is a way to cast a Maybe monad to the data type it could hold. To be able to call it, make sure to have the line `import qualified Data.Maybe as M`. The findIndex function could fail to find an index in some cases, so instead of returning an arbitrary value if it doesn't find the character in the string, it returns `nothing`, otherwise it returns `Just ind` where `ind` is the index. (See the next post for more on monads).

The part after the first comma, `let (bef, aft) = T.splitAt ind x`, defines `bef` to be the first part of the string before the letter in question, and `aft` to be the rest of the string (including the letter)

The part after the second comma, `T.isInfixOf (T.singleton 'b') x`, is the test to see if there are any of the given letter within the string.

The rest of the line of code, `= T.snoc (order (T.append bef (T.tail aft))) 'b'`, concatenates the two parts of the string, before and after the letter together and calls order on that concatenation. Then, the letter itself is tacked on to the end of the result of the recursive call.

The `applyRules` is significantly simpler than `order`. See if you can work through the logic on your own!

<br>

---

## What about Roman Numerals?

<br>

Now that we know about rewrite systems and how to implement them in Haskell, let's apply the same logic to Roman Numerals!

We can say that the equivalency classes for Roman Numeral strings would be determined by the number it represents (i.e. "VV" and "X" would be in the same class) and the normal form of each class would be the "correct" string that represents the number (i.e. "IV" would be the normal form of "IIII")

If no special cases like "IV" or "XC" existed, the rewrite system would be pretty simple, just have rules for the order of the letters, and rules that condense groups of letters into a single letter:

    IV -> VI
    IX -> XI
    ...
    CM -> MC
    IIIII -> V
    VV -> X
    XXXXX -> L
    ...
    DD -> M

If you wanted to implement this in Haskell, the syntax would be the _exact_ same as it is for the other rewrite system, with two functions and everything!

Unfortionately, things aren't that simple. Don't fret though, we can still use what we've done with the simple ordering rules in the more complicated version! The order function can stand as is, but we need a way to swap between the "true" roman numerals with cases like "IV" and our simplified version.

Expanding is super easy (thankfully), although we need to move slightly away from the more abstract world of string rewrite systems and work in the concrete world of Haskell. We need to implement a separate set of rules that are as follows:

    CM -> DCCCC
    CD -> CCCC
    ...
    IX -> VIIII
    IV -> IIII

Let's put those rules into a new function called "expand"

    expand :: T.Text -> T.Text
    expand x
    | T.isPrefixOf (T.pack "CM") x = T.append (T.pack "DCCCC") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "CD") x = T.append (T.pack "CCCC") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "XC") x = T.append (T.pack "LXXXX") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "XL") x = T.append (T.pack "XXXX") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "IX") x = T.append (T.pack "VIIII") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "IV") x = T.append (T.pack "IIII") (expand (T.drop 2 x))
    | not (T.null x) = T.cons (T.head x) (expand (T.tail x))
    | otherwise = T.empty

Notice how we apply the recursion, specifically the second to last case: `not (T.null x)`. This is basically telling the computer to, if the string is not empty, drop the first letter of the string and expand the rest.

The inverse function, contract, is much more complicated. It can be broken up into 4 distinct cases that are repeated with the higher orders of magnitude.

1. Two letters that are half of the next are combined
    - VV -> X
2. Five letters that are one fifth of the next are combined
    - IIIII -> V
3. One letter that's half of the next, four that are one tenth of that letter are combined
    - VIIII -> IX
4. Four letters that are one fifth of the next are combined
    - IIII -> IV

If you were to implement these rules as is, you'd run into some issues when dealing with certain cases. For example, given the same order as listed above, the string "VVIIII" would contract to "VIX". (You can try it yourself to verify)

One way to circumvent that is to add another rule

5. A letter is proceded by one 5 times greater and followed by one 10 times greater, the proceding and following letters are swapped.
    - VIX -> XIV

And finally,

6. None of the previous rules apply, just move on to the next letters until you've contracted everything!

However... These rules _still_ wouldn't lead to a bug-free function! There is a possibility that as the string is contracted, the order of the letters will get messed up and the string won't be contracted correctly. To fix this, for cases 1 and 2, the `order` function needs to be called with the newly contracted letters. This won't ruin the rest of the cases because order will only be called on strings that don't have any of the special cases in them. All the special cases will be "dropped" when case 6 is reached for a given letter. Here's the full function:

    contract :: T.Text -> T.Text
    contract x
    | T.isSuffixOf (T.pack "VV") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "X")))
    | T.isSuffixOf (T.pack "IIIII") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "V")))
    | T.isSuffixOf (T.pack "VIIII") x = contract (T.append (T.dropEnd 5 x) (T.pack "IX"))
    | T.isSuffixOf (T.pack "IIII") x = contract (T.append (T.dropEnd 4 x) (T.pack "IV"))
    | T.isSuffixOf (T.pack "VIX") x = contract (T.append (T.dropEnd 3 x) (T.pack "XIV"))
    | T.isSuffixOf (T.pack "LL") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "C")))
    | T.isSuffixOf (T.pack "XXXXX") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "L")))
    | T.isSuffixOf (T.pack "LXXXX") x = contract (T.append (T.dropEnd 5 x) (T.pack "XC"))
    | T.isSuffixOf (T.pack "XXXX") x = contract (T.append (T.dropEnd 4 x) (T.pack "XL"))
    | T.isSuffixOf (T.pack "LXC") x = contract (T.append (T.dropEnd 3 x) (T.pack "CLX"))
    | T.isSuffixOf (T.pack "DD") x = contract (order (T.append (T.dropEnd 2 x) (T.pack "M")))
    | T.isSuffixOf (T.pack "CCCCC") x = contract (order (T.append (T.dropEnd 5 x) (T.pack "D")))
    | T.isSuffixOf (T.pack "DCCCC") x = contract (T.append (T.dropEnd 5 x) (T.pack "CM"))
    | T.isSuffixOf (T.pack "CCCC") x = contract (T.append (T.dropEnd 4 x) (T.pack "CD"))
    | T.isSuffixOf (T.pack "DCM") x = contract (T.append (T.dropEnd 3 x) (T.pack "MCD"))
    | not (T.null x) = T.snoc (contract (T.init x)) (T.last x)
    | otherwise = T.empty

For the sake of completeness, here's the completed `order` function as well:

    order :: T.Text -> T.Text
    order x
    | let ind = M.fromJust (T.findIndex ('I' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'I') x = T.snoc (order (T.append bef (T.tail aft))) 'I'
    | let ind = M.fromJust (T.findIndex ('V' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'V') x = T.snoc (order (T.append bef (T.tail aft))) 'V'
    | let ind = M.fromJust (T.findIndex ('X' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'X') x = T.snoc (order (T.append bef (T.tail aft))) 'X'
    | let ind = M.fromJust (T.findIndex ('L' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'L') x = T.snoc (order (T.append bef (T.tail aft))) 'L'
    | let ind = M.fromJust (T.findIndex ('C' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'C') x = T.snoc (order (T.append bef (T.tail aft))) 'C'
    | let ind = M.fromJust (T.findIndex ('D' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'D') x = T.snoc (order (T.append bef (T.tail aft))) 'D'
    | let ind = M.fromJust (T.findIndex ('M' ==) x), let (bef, aft) = T.splitAt ind x, T.isInfixOf (T.singleton 'M') x = T.snoc (order (T.append bef (T.tail aft))) 'M'
    | otherwise = T.empty

<br>

---

## Making it a Calculator

<br>

Now that we have the ability to find the normal form of any sequence of roman numeral characters and a way to translate between our simplified version and the real way Roman Numerals work, let's put those functions to work! (Note: for the sake of the following functions, zero is going to be represented with an empty string. That would be confusing for humans to keep track of, but luckily it's no issue for Haskell)

A function that adds two Roman Numerals is simple! All you have to do is expand the two numerals you're adding, concatenate them together, then calculate the normal form by ordering and contracting them.

    addRN :: T.Text -> T.Text -> T.Text
    addRN m n = contract (order (T.append (expand m) (expand n)))

Subtraction and Multiplication are a bit tougher, but once we write a (relatively) simple function that decrements a Roman Numeral. In order to avoid negative numbers, decrementing an empty string should just return an empty string. Try this on your own first! Here's the outline:

    minusOneRN :: T.Text -> T.Text
    minusOneRN x
    | T.isSuffixOf (T.pack "I") x = T.dropEnd 1 x
    ...
    | otherwise = T.empty

HINT: There's a limited number of cases for how a Roman Numeral string can end, and those cases are distinguished by either a single ending character or a pair of characters that are _special_

HINT: Since the function only needs to subtract 1 at any given time, you never need to call it recursively.

HINT: Think about cases like 400-1, 1000-1 and even 9-1. What needs to happen with those cases that's special?

---

Here's the full function:

    minusOneRN :: T.Text -> T.Text
    minusOneRN x
    | T.isSuffixOf (T.pack "I") x = T.dropEnd 1 x
    | T.isSuffixOf (T.pack "IV") x = T.append (T.dropEnd 2 x) (T.pack "III")
    | T.isSuffixOf (T.pack "V") x = T.append (T.dropEnd 1 x) (T.pack "IV")
    | T.isSuffixOf (T.pack "IX") x = T.append (T.dropEnd 2 x) (T.pack "VIII")
    | T.isSuffixOf (T.pack "X") x = T.append (T.dropEnd 1 x) (T.pack "VIV")
    | T.isSuffixOf (T.pack "XL") x = T.append (T.dropEnd 2 x) (T.pack "XXXIX")
    | T.isSuffixOf (T.pack "L") x = T.append (T.dropEnd 1 x) (T.pack "XLIX")
    | T.isSuffixOf (T.pack "XC") x = T.append (T.dropEnd 2 x) (T.pack "LXXXIX")
    | T.isSuffixOf (T.pack "C") x = T.append (T.dropEnd 1 x) (T.pack "XCIX")
    | T.isSuffixOf (T.pack "CD") x = T.append (T.dropEnd 2 x) (T.pack "CCCXCIX")
    | T.isSuffixOf (T.pack "D") x = T.append (T.dropEnd 1 x) (T.pack "CDXCIX")
    | T.isSuffixOf (T.pack "CM") x = T.append (T.dropEnd 2 x) (T.pack "DCCCXCIX")
    | T.isSuffixOf (T.pack "M") x = T.append (T.dropEnd 1 x) (T.pack "CMXCIX")
    | otherwise = T.empty

Using that function, try to come up with functions for subtraction and multiplication!

HINT: These functions are both going to be similar to the multiplication and subtraction functions for successor numbers, if you're familiar with that.

HINT: Think of what special cases exist for multiplication and subtraction.

HINT: x - y = (x-1) - (y-1)

HINT: x \* y = x + (x \* (y-1))

Here are the full functions:

    subtractRN :: T.Text -> T.Text -> T.Text
    subtractRN m n
    | m == T.empty = T.empty
    | n == T.empty = m
    | otherwise = subtractRN (minusOneRN m) (minusOneRN n)

    multRN :: T.Text -> T.Text -> T.Text
    multRN m n
    | (m == T.empty) || (n == T.empty) = T.empty
    | m == T.pack "I" = n
    | n == T.pack "I" = m
    | otherwise = addRN m (multRN m (minusOneRN n))

As a final challenge, write two functions: one that converts from integers to Roman Numerals, and another that coverts from Roman Numerals to integers. This will help you show off your other functions to friends!

[Blog Hub](../index) | [Previous](post3) | [Next](post5)
