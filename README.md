# potato-flow
`potato-flow` is a mono-space text flow-chart editor written in Haskell. It is currently a work in progress and an ALPHA release should be available soon.

# architecture
`potato-flow` is written using [reflex](https://github.com/reflex-frp/reflex) and follows the MVC architecture using. This repository contains the platform-independent model and controller.
The View is connected to the reflex interface defined by `GoatWidget`. The MC/V separation is very strict.

[potato-flow-vty](https://github.com/pdlla/potato-flow-vty) is currently the only view implementation. It is written in [reflex-vty](https://github.com/reflex-frp/reflex-vty) and runs in a terminal yay.

# roadmap

## ALPHA/BETA (current state)
- canvas resize capability
- basic file I/O interface via CLI
- parameter setting capability (view)

## v1
- layer folder support
- proper save/load interface (this is actually implemented in purely in the "view")
- text alignment
- free form text-area input

## v2
- multi-segment line input
- attached line support
- multi-document support
- refactor PFWorkspace to handle non-linear action do/undo operations in preparation for multi-user mode
- UNICODE wide character support


## v3
- multi-user mode
  - ordering service interface
  - basic single client authoritative implementation of ordering service interface (for now)
- scripting
