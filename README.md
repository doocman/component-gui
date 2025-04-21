# aspect-gui
Experimental C++ gui (with ambitions to also support rust in the future.)


# Getting started

See the examples folder for some use cases.

# Design goals

## Component based

Widgets are built around reusable components rather than through inheritance. This to improve code composability.

## GUI only, back-end agnostic

Window management system, user input etc. are not explicitly part of the library, but it should be possible to setup an arbitrary backend that provides the necessary infrastructure.
