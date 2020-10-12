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

[Blog Hub](../index) | [Next](week2)
