# tinytools software architecture

This file documents tinytools software architecture. 

tinytools consists of the M (as in MVC) of the tinytools app. The VC implementations (separate repository) are connected via a Reflex interface in `GoatWidget`

## Terminology

- Document:
- Scene: same as document 
- Canvas: the 2D interactable area of tinytools
- Owl: an element in the scene
- Layers: the layer system which is a logical view into the scene state


## State

The main tinytools hierarchy looks something like this

```
               ╔GoatState══════════════════╗                                                                                                                                              
               ║-scene info                ║                                                                                                                                              
               ║-render state              ║                                                                                                                                              
               ║-input state               ║                                                                                                                                              
               ║                           ║                                                                                                                                              
               ║╔OwlPFWorkspace═══════════╗║                                                                                                                                              
               ║║-do/undo stack           ║║                                                                                                                                              
               ║║                         ║║                                                                                                                                              
               ║║╔OwlState═══════════════╗║║                                                                                                                                              
               ║║║-canvas                ║║║                                                                                                                                              
               ║║║                       ║║║                                                                                                                                              
               ║║║╔OwlTree══════════════╗║║║                                                                                                                                              
               ║║║║tree of all elements ║║║║                                                                                                                                              
               ║║║║or "owls" in the doc ║║║║                                                                                                                                              
               ║║║╚═════════════════════╝║║║                                                                                                                                              
               ║║╚═══════════════════════╝║║                                                                                                                                              
               ║╚═════════════════════════╝║                                                                                                                                              
               ╚═══════════════════════════╝    
```

Goat.hs is the effective entry point into `tinytools` with `foldGoatFn :: GoatCmd -> GoatState -> GoatState` as the primary state update function.

- NOTE this will eventually be refactored into individual update methods of type  `Endo GoatState`
- NOTE `GoatState` will eventually be refactored to contain a list of `GoatTabs`, one for each document tab.

`OwlPFWorkspace` contains the `LLamaStack` which contains information on `OwlState` update operations or `Llama`s 

`OwlState` contains a `SCanvas` which represents the visible area of the document.

`OwlTree` is a specialized tree object representing the elements, called "Owls" in the document. The tree is indexed by `REltId` which is a unique integer id for each owl.

## Reflex Interface

The application state exposes a `reflex` interfaces via `GoatWidget` which essentially wraps a `GoatState` and wires the `reflex` inputs to update the state and connects the state to various outputs.

The inputs on the left in the diagram below are reflex `Event`s and the outputs on the right or reflex `Dynamic`s. 
                                                                                                                                                                                          
                                                                                                                                                                                      
```                                                                                                                                                                                           
                                         ╔GoatWidget═════════════════╗                                                                                                                    
            ╔═════════kb/mouse══════════>║                           ║═════════owl state═══════════╗                                                                                      
            ║                            ║                           ║                             ║                                                                                      
            ║════════screen size════════>║                           ║════════canvas info═════════>║                                                                                      
            ║                            ║                           ║                             ║                                                                                      
            ║═══════canvas resize═══════>║                           ║══════rendered scene════════>║                                                                                      
            ║                            ║                           ║                             ║                                                                                      
            ║═══════style changes═══════>║                           ║═════════selection══════════>║                                                                                      
            ║                            ║                           ║                             ║                                                                                      
            ║═════════load file═════════>║*--------------*           ║═══════handler info═════════>║                                                                                      
            ║                            ║|GoatState     |           ║                             ║                                                                                      
            ║═══════════other═══════════>║|              |           ║═══════════other════════════>║                                                                                      
            ║                            ║*--------------*           ║                             ║                                                                                      
            ║                            ╚═══════════════════════════╝                             ║                                                                                      
            ║                                                                                      ║                                                                                      
            ║                                                                                      ║                                                                                      
            ║                            █tinytools-myviewcontroller██                             ║                                                                                      
            ║                            █ ╔═════╗╔═════════════════╗█                             ║                                                                                      
            ║                            █ ║...  ║║                 ║█                             ║                                                                                      
            ║                            █ ║     ║║  ╔════╗         ║█                             ║                                                                                      
            ║                            █ ║     ║║  ║    ║═════╗   ║█                             ║                                                                                      
            ║                            █ ║     ║║  ╚════╝     ║   ║█                             ║                                                                                      
            ║                            █ ║     ║║             v   ║█                             ║                                                                                      
            ╚════════════════════════════█ ║═════║║ ╔boop══╗ ╔════╗ ║█<════════════════════════════╝                                                                                      
                                         █ ║     ║║ ║@@@@@@║ ║    ║ ║█                                                                                                                    
                                         █ ║     ║║ ║@@@@@@║ ╚════╝ ║█                                                                                                                    
                                         █ ║═════║║ ║@@@@@@║        ║█                                                                                                                    
                                         █ ║     ║║ ║@@@@@@║        ║█                                                                                                                    
                                         █ ║     ║║ ╚══════╝        ║█                                                                                                                    
                                         █ ╚═════╝╚═════════════════╝█                                                                                                                    
                                         █████████████████████████████                                                                                                                                                      
                                                                                                                                                                                                                                 
```                           


## Input

Some mouse and keyboard input is handled within tinytools. In particular, the canvas and layers input are handled by tinytools. 

Most mouse and keyboard input is managed by instances of the  `Handler` class. The layers and canvas portions can each only have 1 handler each at a time.
If a `Handler` method processes the input, it returns a `PotatoHandlerOutput` which contains information on how to update the scene as well as a new `Handler`.

## Rendering

tinytools contains 3 main optimizations for rendering

- a broadphase culling step
- incremental rendering of only parts that changed
- caching

### BroadPhase

The broadphase culling step is managed by the `BroadPhase` class which uses axis aligned bounding boxes (AABBs) to cull out anything outside the regions to be re-rendered.

NOTE `BroadPhase` uses a linear search implementation and will be upgrade to a kd-tree implementation if that should ever make a meaningful difference.

### Incremental Rendering

Each update operation returns a list of modified elements and only the areas on the screen that are impacted by the modified elements are rerenders.

### Caching

`GoatState` stores a `RenderCache` object which is a map from each owl's id into a `OwlItemCache`. 

Each time there is a change, the `RenderCache` is cleared on the objects that got changed, and will get regenerated in the render step.

`OwlItemCache` include caches specific to each owl type as well as a generic `PreRender` cache that applies to all types. The `PreRender` cache is also necessary for properly rendering unicode wide chars

The `RenderCache` is also used by the handlers for visual based interface operations.

## Testing

tinytools has many many unit tests (UTs). They come in 3 types:

- direct testing of some functions e.g. ``plus 1 1 `shouldBe` 2``
- testing via `GoatTester` monad 
- DEPRECATED testing via `GoatWidget`

### GoatTester
`GoatTester` is a `State` monad that contains a `GoatState` and an interface to sensibly update, read, and test the state. Mosts tests thus consist of several operations (as if one were using the app) before checking if the state is as expected.

### GoatWidget testing
These tests go through the Reflex interface which is very unecessary and have since been replaced by `GoatTester`



## Known Issues

- bugs in Potato.Data.Text.Zipper 
  - handling cursore position based no DisplayLinesWithAlignment is broken
  - unicode wide chars cause crashes
- `foldGoatFn` evaluated multiple times per input event. Specifically, any input event to GoatWidget will cause `foldGoatFn` to be evaluated at least twice.

## Future Plans
There are many future additions we'd like to make to tinytools. The most relevant ones to be aware of are:

- multi-document support, in particular, part of `GoatState` will be separated out into `[GoatTab]`
- multiplayer support
  - we will use a trusted/permissioned decentralized consensus algorithm for this
- refactor `OwlTree` to be a tree and `OwlPartialment` to be a zipper rather than a list of ids such that we can remove most partial functions in Owl.hs
  - `OwlTree` still needs to support id lookup as some things will continue to use id references
    - attachments
    - over the wire changes (when multiplayer support is added)
- style objects to be switched over to swatch references rather than stored per-owl
- see ::FUTURE FEATURES:: in TODO.txt for more stuff :D

## FAQ

- why are things randomly named after animals like 🐐🦙🦉?
  - I was having a hard time coming up with unambiguous terminology following more conventional naming patterns. You can see use more conventional naming in some places.
  - I like animals