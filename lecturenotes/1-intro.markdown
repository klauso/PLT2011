Introduction
============

These are lecture notes for the "Programming Languages and Types" at the University of Marburg

loosely based on "Programming Languages: Application and Interpretation" by
Shriram Krishnamurthi

Please send comments or errors in these notes via email to Klaus Ostermann.
My email address can be found on my webpage.


Why are you here?
=================

This is an advanced programming course that focuses on the design and semantics
of programming languages.

The goals of this course are:

* To change the way you solve problems in programming: By principled design,
  with a rich toolbox of abstract concepts, rather than by tinkering.
* Enable you to think beyond the currently fashionable programming languages.
* Enable you to evaluate and discuss programming languages in a principled
  way.
* To show you where computer languages come from and potential future
  directions.
* To demonstrate why languages should be regarded as the ultimate form of
  abstraction.
* To teach you how to recognize abstractions and how to turn them into a
  language design.
* Teach you different ways how to realize a language design and make it
  practically useful.
* To convey a sense of aesthetics in programming.
* To introduce you into one of the most beautiful theories in theoretical
  computer science.
* To turn you into a better programmer!

Non-goals of this course are:

* Learn X different programming languages. Rather, you'll learn to decompose a
  PL into its features and discuss the merits of the features independently of the
  concrete PL.
* Write parsers or implement full-fledged compilers (although we will touch some
  compiler issues).
* Turn you into an evangelist for a particular PL.


How will we get there?
======================

We will typically explain language features in two steps. First, we'll give an
informal introduction and discuss some examples to get some experience with it.
Then we will distill the core of the feature into an executable interpreter.
Later, when we have more experience in formalizing language design into
interpreters, we will also use formal mathematical notation.

We will not just present the design of these features as if had disappeared out
of the blue sky. Rather, we will discuss the process and trade-offs (and
potential mistakes and pitfalls) that led to these designs. We will also
discuss how these features appear in various available programming languages.

We will mainly use the programming language Scala to write interpreters. Scala is
sufficiently powerful to allow concise and elegant interpreters; on the other hand,
it is sufficiently mature and popular for industrial usage. It also has variants
of many of the features we will discuss. The book by Krishnamurthi, on which we loosely
base the first part of the course, uses a different language, Racket. You are also
welcome to use Racket instead of (or in addition to) Scala. Racket embodies
an important tradition of PL design that is worth knowing, hence we encourage you,
independently of this course, to study it!


Topics for class discussion:
============================

* What would you like to learn in this course? What are your expectations?
* Which programming languages do you know? Which features of these languages do you
  like or dislike, and why?
* How can we evaluate programming languages?
* Is the choice of programming language for a programming project significant?
* Are interpreters a good way to capture the meaning of a PL? How do they compare
  to informal descriptions, formal specifications, and compilers?
* How important is the syntax of a PL?