# tinytools software architecture

This file documents tinytools software architecture. 

tinytools consists of the M (as in MVC) of the tinytools app. The VC implementations (separate repository) are connected via a Reflex interface in `GoatWidget`

## Terminology

- Document: the thing you are working on in tinytools, I don't know how else to describe this 🤣
- Scene: same as document 
- Canvas: the 2D interactable area of tinytools
  - Canvas also refers to the designated area of the document that we care about (TODO explain better)
- Layers: the logical list view of elements and folders in the scene
- Owl: an element in the scene
- Element: same as owl
- Layers: the layer system which is a logical view into the scene state
- OwlState: the data structure representing the document state


## State

The main tinytools state hierarchy looks something like this:

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

Please see comments in `GoatWidget` and `GoatWidgetConfig` in GoatWidget.hs for more details.
For an example setup, see `Potato.Flow.Vty.Main` in the tinytools-vty project

## Input

Some mouse and keyboard input is handled within tinytools. In particular, the canvas and layers input are handled by tinytools. 

Mouse input is reported as click streams, see `LMouseData`
Mouse input must be reported separately for the Layers and canvas area by setting the `_lMouseData_isLayerMouse` field. All input sequences in one area must end with a mouse release (`_lMouseData_isRelease = True`) before switching to the other area or it will assert. (in retrospect, this is could have been done a little more clear/safe)


### Handlers

Most mouse and keyboard input is managed by instances of the  `Handler` class. The layers and canvas portions can each only have 1 handler each at a time.
If a `Handler` method processes the input, it returns a `PotatoHandlerOutput` which contains information on how to update the scene as well as a new `Handler`.

#### Handler Lifecycle

The handler lifecycle is important and has a few subtle/idiosyncratic requirements. Below is a description of the key state transition of the handler lifecycle (for both Layers and Canvas handlers, although some scenarios described really only apply to the Canvas handler)

- there is always a handler
- handlers may be active or inactive
  - an example of an active handler is when `BoxHandler; is in the middle of resizing an object via mouse drag
- after processing input, handlers return some output action as well as a maybe a new handler
  - if no new handler is returned, the handler does not process the input and the input may be processed in other ways
    - for example, if a mouse down input is not handled by the canvas handler, then a `SelectHandler` is created (replacing the other handler) to process the input
  - If a new handler is returned, it is set as the handler
- after a new object(s) is created locally
  - select the newly created object(s)
    - NOTE this is SUPER important, as the handler that just created the element expects it to be selected to continue modifying it
  - if the current handler is not active, create a new handler based on the newly created object
  - if the current handler is active, do nothing, trust that the current handler knows what it's doing
- after any change to the doc's OwlState, refresh the handler, which may produce a non-trivial output
- if Esc is pressed, cancel the current handler, which may produce a non-trivial output 

## Rendering

tinytools contains 3 main optimizations for rendering

- a broadphase culling step
  - This is managed by the state object `BroadPhaseState` 
  - Note, the current implementation just does a linear cull but someday it will use a proper KD tree.
- incremental rendering of only parts that changed
  - Each change operation produces a list of boxes indicating the areas that changed and only those areas are re-rendered
  - Only the visible screen region is rendered. The screen region can be set with `_goatWidgetConfig_canvasRegionDim`
- caching
  - each object has a generic cache which is the post rendered unicode output as well as an optional specific cache based on its needs

The rendered scene available through `_goatWidget_renderedCanvas` and the rendered selection is available through `_goatWidget_renderedSelection`.

The handler gizmos (the things you use to manipulate the scene or layers) are available through `_goatWidget_handlerRenderOutput` and `_goatWidget_layersHandlerRenderOutput`


### BroadPhase

The broadphase culling step is managed by the `BroadPhase` class which uses axis aligned bounding boxes (AABBs) to cull out anything outside the regions to be re-rendered.

- NOTE `BroadPhase` uses a linear search implementation and will be upgrade to a kd-tree implementation if that should ever make a meaningful difference.

### Incremental Rendering

Each update operation returns a list of modified elements and only the areas on the screen that are impacted by the modified elements are rerenderered. 

### Caching

`GoatState` stores a `RenderCache` object which is a map from each owl's id into a `OwlItemCache`. 

Each time there is a change, the `RenderCache` is cleared on the objects that got changed, and will get regenerated in the render step.

`OwlItemCache` include caches specific to each owl type as well as a generic `PreRender` cache that applies to all owl types. The `PreRender` cache is also used for rendering unicode wide chars.

The `RenderCache` is also used by the handlers for visual based interface operations.

## Testing

tinytools has many many unit tests (UTs). They come in 3 types:

- direct testing of some functions e.g. ``plus 1 1 `shouldBe` 2``
- testing via `GoatTester` monad 
- DEPRECATED testing via `GoatWidget`

### GoatTester
`GoatTester` is a `State` monad that contains a `GoatState` and an interface to sensibly update, read, and test the state. Mosts tests thus consist of several operations (as if one were using the app) before checking if the state is as expected.

### GoatWidget testing
These tests go through the Reflex interface which is very unecessary and have since been replaced by `GoatTester`.

## Known Issues

- `foldGoatFn` evaluated multiple times per input event. Specifically, any input event to GoatWidget will cause `foldGoatFn` to be evaluated at least twice.

## Future Plans
There are many future additions we'd like to make to tinytools. The most relevant ones to be aware of are:

- better styling tools
  - style objects to be switched over to swatch references rather than stored per-owl
  - user defined swatch pallete
- glyph widget ༼ つ ◕_◕ ༽つ
- more primitive shapes...
  - ellipse
  - parallelogram
- multi-document support, in particular, part of `GoatState` will be separated out into `[GoatTab]`
- multiplayer support
  - we will use a trusted/permissioned decentralized consensus algorithm for this
- refactor `OwlTree` to be a tree and `OwlPartialment` to be a zipper rather than a list of ids such that we can remove most partial functions in Owl.hs
  - `OwlTree` still needs to support id lookup as some things will continue to use id references
    - attachments
    - over the wire changes (when multiplayer support is added)
- see ::FUTURE FEATURES:: in TODO.txt for even more stuff :D

## FAQ

- why are things randomly named after animals like 🐐🦙🦉?
  - I was having a hard time coming up with unambiguous terminology following more conventional naming patterns. You can see use more conventional naming in some places.
  - I like animals
- why are so many things prefixed with potato
  - some animals eat potatoes 🥔