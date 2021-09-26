[![CircleCI](https://circleci.com/gh/pdlla/potato-flow.svg?style=svg)](https://circleci.com/gh/pdlla/potato-flow)

# potato-illustrator
`potato-illustrator` is a mono-space text flow-chart editor written in Haskell. It is currently a work in progress and an ALPHA release should be available soon.

# architecture
`potato-illustrator` is written using [reflex](https://github.com/reflex-frp/reflex) and follows the MVC architecture using. This repository contains the platform-independent model and controller.
The View is connected to the reflex interface defined by `GoatWidget`. The MC/V separation is very strict.

[potato-illustrator-vty](https://github.com/pdlla/potato-illustrator-vty) is currently the only view implementation. It is written in [reflex-vty](https://github.com/reflex-frp/reflex-vty) and runs in a terminal yay. Please see [potato-illustrator-vty](https://github.com/pdlla/potato-illustrator-vty) if you'd like to try it out.

# features (completed)
- sophisticated hierarchical layer system
- transaction based undo stack
- several configurable primitives including boxes, lines and text boxes

# roadmap

## alpha
- save/load/export interface
- multi-segment line input
- free form text-area input

## v1
- UNICODE wide character support
- UNICODE glyph widget

## v2
- attached line support
- multi-document support
- refactor handle non-linear action do/undo operations in preparation for multi-user mode

## v3
- graphene clusters support
- multi-user mode
  - ordering service interface
  - basic single client authoritative implementation of ordering service interface (for now)
- scripting
