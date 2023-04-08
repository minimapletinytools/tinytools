[![CircleCI](https://circleci.com/gh/pdlla/potato-flow.svg?style=svg)](https://circleci.com/gh/pdlla/potato-flow)

# tinytools
`tinytools` is a mono-space unicode flow-chart editor written in Haskell.

# architecture
`tinytools` follows a strict MVC architecture. This repository contains the platform-independent model and controller.
The view/controller is connected to the model using a [reflex](https://github.com/reflex-frp/reflex) interface defined by `GoatWidget`.

[tinytools-vty](https://github.com/pdlla/tinytools-vty) is currently the only view implementation. It is written in [reflex-vty](https://github.com/reflex-frp/reflex-vty) and runs in a terminal yay. Please see [tinytools-vty](https://github.com/pdlla/tinytools-vty) if you'd like to try it out.

# features (completed)
- sophisticated hierarchical layer system
- transactional operations and change history
- several highly configurable primitives including boxes, lines and text boxes
- basic document save/load/export functionality

# roadmap

## v1
- UNICODE wide character support (currently blocked by issues in TextZipper)
- UNICODE glyph widget

## v2
- multi-document support
- refactor handle non-linear action do/undo operations in preparation for multi-user mode

## v3
- grapheme clusters support (blocked by terminal support which is currently extremely inconsistent or non-existent)
- multi-user mode
  - ordering service interface
  - basic single client authoritative implementation of ordering service interface (for now)
- scriptable command interface


# Contribution Guide

Help wanted! I will of course review any PR. For large or small ideas, it would be best to drop me an email first at chippermonky at gmail dot com

Below are a list of tasks that I think would be extra good projects to work on

- CI scripts for creating binary releases
- add text selection support to current `TextZipper` implementation and fix bugs
- refactoring 😑
