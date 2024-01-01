# tinytools
`tinytools` is a mono-space unicode diagram editor written in Haskell.

# architecture
`tinytools` follows a strict MVC architecture. This repository contains the model and controller.
The view/controller is connected to the model using a [reflex](https://github.com/reflex-frp/reflex) interface defined by `GoatWidget`.

[tinytools-vty](https://github.com/minimapletinytools/tinytools-vty) is currently the only view implementation. It is written in [reflex-vty](https://github.com/reflex-frp/reflex-vty) and runs in a terminal yay. Please see [tinytools-vty](https://github.com/minimapletinytools/tinytools-vty).

# features
- modern and intuitive UI/UX
- several highly configurable primitives including boxes, lines and text boxes
- sophisticated hierarchical layer system
- transactional operations and change history
- basic document save/load/export functionality

# roadmap

`tinytools`` is still actively being worked on. Here are some things to look out for in the future:

- more native shapes and custom shape support
- glyph widget for easy emoji or ASCII art insertion
- multi-character ASCII font support 
- multi-document support
- multi-user support
- web based frontend
