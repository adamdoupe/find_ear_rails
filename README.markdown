# Static Execution After Redirect Detector for Ruby on Rails

This is a tool to statically detect Execution After Redirect (EAR)
bugs in Ruby on Rails applications. It was initially developed for a
paper in progress.

## What is an EAR?
[Bryce Boe][bboe-ear] (my co-author) has a good description on his blog, and I have [my own EAR explination][me-ear]. But here's the short version:

## How to compile?

These steps are for Ubuntu.

First need the following packages:

* ocaml
* omake
* ocaml-findlib
* ocaml-libs
* ocaml-native-compilers
* ocaml-tools
* libgetopt-ocaml-dev
* libocamlgraph-ocaml-dev
* libounit-ocaml-dev

And get and compile the following package:

* libsyck (v 7.0) https://github.com/indeyets/syck

Run:
    omake

Now you have a fresh and new find\_ear\_rails to play with! To run, just supply a rails project directory as the first argument.



[me-ear]: http://adamdoupe.com/overview-of-execution-after-redirect-web-appl
[bboe-ear]: http://www.bryceboe.com/2010/12/09/ucsbs-international-capture-the-flag-competition-2010-challenge-6-fear-the-ear/
