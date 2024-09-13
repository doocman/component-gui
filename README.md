# component-gui
Currently me testing a component rather than inheritance-based GUI. The ambition is to have it functioning for both rust and c++.

# Getting started

See the examples folder for some use cases.

# Design goals

## Component based

Widgets are built around reusable components rather than through inheritance. This to improve code composability.

## GUI only, back-end agnostic

Window management system, user input etc. are not explicitly part of the library, but it should be possible to setup an arbitrary backend that provides the necessary infrastructure.
