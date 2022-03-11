# Bogue with effects

This is an experiment at implementing a GUI framework (and the widgets specifically) using effect handlers. It is based on [Bogue](https://github.com/sanette/bogue) although most of the code has been rewritten or removed. In addition, the idea behind this project comes from the [Concur](https://ajnsit.github.io/concur-documentation/title-page.html) framework.

For this we represent the UI as a tree of widgets. Every leaf represents some basic widget whereas the inner nodes manage the positioning, delegation of events and display of their children. This can be seen as somewhat similar to a distributed scheduler. When a widget wants to react to an event it will suspend its computation using an effect. This effect is handled by its parent which stores the continuation of the child and notifies its parent of the awaited events. This continues all the way to the root where the computation stops and the UI rendering finishes.

Once an event occurs it traverses this tree down to the relevant widget which handles it. After handling it, the widget again suspends its computation and awaits the next event, repeating the process.

Currently this project is in a pretty barebones state. Some basic widgets have been implemented (found in src/widgets/) and the basic functionality is demonstrated in examples/effects.ml.

## Project structure
* examples/effects.ml: Currently the only example
* src/widgets/: Several simple widgets
* src/widgets/base.ml: Sets up some basic concepts for the other widgets
* src/lib/layout.ml: The object representing a window

The remaining files are mostly irrelevant for this and quite similar to Bogue. Note that even the files mentioned above contain obsolete code and/or are more complicated than they need to be. This is simply a consequence of the rather aggressive refactoring/removal of features from Bogue.

## Remaining major issues
* Handle resizing of the window correctly. How can we elegantly specify the size of widgets? This becomes especially problematic once not enough space is available.
* Separate event listeners into global and local. Right now only one widget can listen to keyboard inputs and mouse events can only be listened to if they occur above the corresponding widget
* Explore popups and windows. Perhaps even transparency. Popups in Bogue could be shown in the same window but this seems to be additional unnecessary complexity. Should two widgets be allowed to overlap? How should they behave and displayed?
* Simplify composition. Right now every logic element must be wrapped into a Single.t or inherit from some other widget. This seems overly complicated. In addition lists are unsuitable as argument for Rows as their type must be identical and casting the objects is always explicit. Perhaps all of these issues can be solved with an infix operator? A ppx is also an option, although they seem to be overkill for this.
* Transition to GLFW as it is more lightweight and I would like to integrate this GUI into a game written with raylib. Interop between SDL and GLFW seems to be impossible. After this transition rendering can likely be optimised quite significantly.

## Installing

This package uses `ppx_effects` which is only available for 4.12.0+domains. You need to create a new opam switch and install (at least) the following packages:
`tsdl` `tsdl-image` `tsdl-ttf` `ppx_deriving` `ppx_effects`
