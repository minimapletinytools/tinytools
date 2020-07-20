{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.New.Testing (
  simpleSBox
  , FCmd(..)
  , randomActionFCmd
  , setup_network
  , step_state_network
) where

import           Relude                 hiding (empty, fromList)

import           Reflex
import           Reflex.Test.Host

import           Data.Constraint.Extras (Has')
import           Data.Default           (def)
import           Data.Dependent.Sum     ((==>))
import qualified Data.IntMap.Strict     as IM
import qualified Data.List              as L (take, (!!))
import qualified Data.List.Index        as L
import           Data.Maybe             (fromJust)
import           Data.Tuple.Extra
import qualified Text.Show

import qualified Control.Monad.Random   as R
import           System.Random.Shuffle

import           Potato.Flow


simpleSBox :: SBox
simpleSBox = SBox (LBox (V2 5 5) (V2 5 5)) def

data FCmd =
  FCNone
  -- adding a SEltFolderStart automatically adds corresponding SEltFolderEnd
  | FCAddElt LayerPos SElt
  | FCDeleteElt LayerPos
  | FCModify LayerPos Controller
  | FCModifyMany [(LayerPos, Controller)]
  | FCResizeCanvas DeltaLBox
  | FCUndo
  | FCRedo
  | FCSave
  | FCLoad SPotatoFlow

  | FCCustom_Add_SBox_1
  | FCCustom_CBox_1 LayerPos
  deriving (Eq)

instance Show FCmd where
  show FCNone                        = "FCNone"
  show (FCAddElt lp SEltFolderStart) = "FCAdd Folder " <> show lp
  show (FCAddElt lp _)               = "FCAdd " <> show lp
  show (FCDeleteElt lp)              = "FCDelete " <> show lp
  show FCUndo                        = "FCUndo"
  show FCRedo                        = "FCRedo"
  show _                             = "other"

setup_network:: forall t m. (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => Event t FCmd -> TestGuestT t m (PFOutput t)
setup_network ev = mdo
  let
    --ev = traceEvent ("aoeu") ev'
    addEv = fforMaybe ev $ \case
      FCAddElt p x -> case x of
        SEltFolderStart -> Nothing
        SEltFolderEnd   -> error "can not explicity add SEltFolderEnd"
        _               -> Just (p, SEltLabel "blank" x)
      FCCustom_Add_SBox_1 -> Just (0, SEltLabel "customsbox" (SEltBox simpleSBox))
      _           -> Nothing

    addFolderEv = fforMaybe ev $ \case
      FCAddElt p x -> case x of
        SEltFolderStart -> Just (p, "somefolder")
        _               -> Nothing
      _ -> Nothing

    removeEv = fforMaybe ev $ \case
      FCDeleteElt p -> Just [p]
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCModify p c -> do
        pFState <- sample beh_pFState
        let (rid, _, SEltLabel _ selt) = fromJust . pFState_getSuperSEltByPos p $ pFState
        return . Just $ IM.singleton rid c
      FCModifyMany pcs -> do
        pFState <- sample beh_pFState
        let sseltls = map (\(p,c) -> (pFState_getSuperSEltByPos p $ pFState, c)) pcs
        return . Just . IM.fromList
          . map (\((rid,_,_),c) -> (rid, c))
          . map (\(mseltl, c) -> (fromJust mseltl, c))
          $ sseltls
      FCCustom_CBox_1 p -> do
        pFState <- sample beh_pFState
        let (rid, _, SEltLabel _ selt) = fromJust . pFState_getSuperSEltByPos p $ pFState
        let
          cbox = CBox {
              _cBox_deltaBox    = DeltaLBox (V2 1 1) (V2 5 5)
            }
        return . Just $ selt `deepseq` IM.singleton rid (CTagBox ==> cbox)
      _              -> return Nothing
    resizeCanvasEv = fforMaybe ev $ \case
      FCResizeCanvas x -> Just x
      _ -> Nothing
    redoEv = fforMaybe ev $ \case
      FCRedo -> Just ()
      _      -> Nothing
    undoEv = fforMaybe ev $ \case
      FCUndo -> Just ()
      _      -> Nothing
    loadEv = fforMaybe ev $ \case
      FCLoad x -> Just x
      _      -> Nothing
    saveEv = fforMaybe ev $ \case
      FCSave -> Just ()
      _      -> Nothing

    pfc = PFConfig {
        _pfc_addElt     = addEv
        , _pfc_addFolder = addFolderEv
        , _pfc_removeElt  = removeEv
        , _pfc_manipulate = manipEv
        , _pfc_resizeCanvas = resizeCanvasEv
        , _pfc_undo       = undoEv
        , _pfc_redo       = redoEv
        , _pfc_load = loadEv
        , _pfc_save = saveEv

        , _pfc_moveElt = never
        , _pfc_paste = never
      }
  pfo <- holdPF pfc
  let
    beh_pFState = current . _pfo_pFState $ pfo
  return pfo


-- | make sure to tick with 'FCNone' to ensure output behavior is most recent
step_state_network :: forall t m.
  (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t () FCmd -> TestGuestT t m (AppOut t () (SPotatoFlow)))
step_state_network AppIn {..} = do
  pfo <- setup_network _appIn_event
  return
    AppOut {
      _appOut_behavior = constant ()
      , _appOut_event = _pfo_saved pfo
    }


isElement :: SEltLabel -> Bool
isElement (SEltLabel _ selt) = case selt of
  SEltNone        -> False
  SEltFolderStart -> False
  SEltFolderEnd   -> False
  _               -> True

randomXY :: (R.MonadRandom m) => m XY
randomXY = do
  x <- R.getRandomR (-99999, 99999)
  y <- R.getRandomR (-99999, 99999)
  return $ V2 x y

randomActionFCmd ::
 (R.MonadRandom m, Has' Show CTag Identity)
 => Bool -> SEltTree -> m FCmd
randomActionFCmd doundo stree = do
  let
    nElts = length stree
    --eltsOnly = filter (isElement . snd) $  L.indexed stree
    eltsOnly = L.indexed stree
    startCmd = if doundo then 0 else 2
  rcmd <- if null eltsOnly
    then return (2 :: Int)
    else R.getRandomR (startCmd, 5)
  case rcmd of
    0 -> return FCUndo
    1 -> return FCRedo
    -- add a new element or folder
    2 -> do
      pos <- R.getRandomR (0, nElts)
      stype <- R.getRandomR (0, 3 :: Int)
      p1 <- randomXY
      p2 <- randomXY
      case stype of
        0 -> return $ FCAddElt pos $ SEltBox
          SBox {
            _sBox_box = LBox p1 p2
            , _sBox_style = def
          }
        1 -> return $ FCAddElt pos $ SEltLine
          SLine {
            _sLine_start = p1
            , _sLine_end = p2
            , _sLine_style = def
          }
        2 -> return $ FCAddElt pos $ SEltText
          SText {
            _sText_box = LBox p1 p2
            , _sText_text = "moo"
            , _sText_style = def
          }
        3 -> return $ FCAddElt pos $ SEltFolderStart
        _ -> undefined
    -- resize the canvas
    3 -> do
      p1 <- randomXY
      p2 <- randomXY
      return $ FCResizeCanvas $ DeltaLBox p1 p2
    -- modify an existing element
    _ -> do
      -- TODO must delete matching folder pairs
      -- just one random elements
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      let (deletePos, (SEltLabel _ _)) = eltsOnly L.!! rindex

      -- many random elements
      shuffled <- shuffleM eltsOnly
      -- TODO for delete you don't want to delete too many otherwise you'll always end up with like no elements
      -- i.e. prob want weighted random
      nTake <- R.getRandomR (1, length eltsOnly)
      let randomElts = L.take nTake shuffled

      case rcmd of
        4 -> return $ FCDeleteElt deletePos
        5 -> fmap FCModifyMany . forM randomElts $ \(pos, (SEltLabel name selt)) -> do
          p1 <- randomXY
          p2 <- randomXY
          cflag <- R.getRandomR (0,10 :: Int)
          case cflag of
            0 -> return $ (,) pos $ CTagBoundingBox ==> CBoundingBox {
                _cBoundingBox_deltaBox = DeltaLBox p1 p2
              }
            1 -> do
              newName <- show <$> R.getRandomR (0, 1000000 :: Int)
              return $ (,) pos $ CTagRename ==>
                CRename {
                  _cRename_deltaLabel = (name, newName)
                }
            _ -> case selt of
              SEltBox _ -> return $ (,) pos $ CTagBox ==>
                CBox {
                  _cBox_deltaBox = DeltaLBox p1 p2
                }
              SEltLine _ -> return $ (,) pos $ CTagLine ==>
                CLine {
                  _cLine_deltaStart = p1
                  , _cLine_deltaEnd = p2
                }
              SEltText (SText _ before _) -> return $ (,) pos $ CTagText ==>
                CText {
                  _cText_deltaBox = DeltaLBox p1 p2
                  , _cText_deltaText = (before, "meow meow")
                }
              -- TODO maybe add a CTagDoNothing?
              _ -> return $ (,) pos $ CTagBoundingBox ==> CBoundingBox {
                  _cBoundingBox_deltaBox = DeltaLBox p1 p2
                }
        _ -> error "this should never happen"
