-- TODO rename to EntryTesting

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Entry.Testing (
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
import qualified Data.Sequence          as Seq
import qualified Text.Show

import qualified Control.Monad.Random   as R
import           System.Random.Shuffle

import           Potato.Flow
import           Potato.Flow.Layers


simpleSBox :: SBox
simpleSBox = SBox (LBox (V2 5 5) (V2 5 5)) def

data FCmd =
  FCNone
  -- adding a SEltFolderStart automatically adds corresponding SEltFolderEnd
  | FCAddElt LayerPos SElt
  | FCDeleteElts [LayerPos]
  | FCModify LayerPos Controller
  | FCModifyMany [(LayerPos, Controller)]
  | FCResizeCanvas DeltaLBox
  | FCMove ([LayerPos], LayerPos)
  | FCCopy [LayerPos]
  | FCPaste LayerPos
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
  show (FCDeleteElts lp)             = "FCDelete " <> show lp
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
        SEltFolderStart -> Nothing -- handled by addFolderEv
        SEltFolderEnd   -> error "can not explicity add SEltFolderEnd"
        _               -> Just (False, (p, SEltLabel "blank" x))
      FCCustom_Add_SBox_1 -> Just (False, (0, SEltLabel "customsbox" (SEltBox simpleSBox)))
      _           -> Nothing

    addFolderEv = fforMaybe ev $ \case
      FCAddElt p x -> case x of
        SEltFolderStart -> Just (p, "somefolder")
        _               -> Nothing
      _ -> Nothing

    deleteEv = fforMaybe ev $ \case
      FCDeleteElts p -> Just p
      _              -> Nothing
    manipEv = flip push ev $ \case
      FCModify p c -> do
        pFState <- sample beh_pFState
        let (rid, _, SEltLabel _ _) = fromJust . pFState_getSuperSEltByPos pFState $ p
        return . Just $ (False, IM.singleton rid c)
      FCModifyMany pcs -> do
        pFState <- sample beh_pFState
        let sseltls = map (\(p,c) -> (pFState_getSuperSEltByPos pFState p, c)) pcs
        return . Just $ (False, IM.fromList
          . map (\((rid,_,_),c) -> (rid, c))
          . map (\(mseltl, c) -> (fromJust mseltl, c))
          $ sseltls)
      FCCustom_CBox_1 p -> do
        pFState <- sample beh_pFState
        let (rid, _, SEltLabel _ selt) = fromJust . pFState_getSuperSEltByPos pFState $ p
        let
          cbox = CBox {
              _cBox_deltaBox    = DeltaLBox (V2 1 1) (V2 5 5)
            }
        return . Just $ (False, selt `deepseq` IM.singleton rid (CTagBox ==> cbox))
      _              -> return Nothing
    resizeCanvasEv = fforMaybe ev $ \case
      FCResizeCanvas x -> Just x
      _ -> Nothing
    moveEv = fforMaybe ev $ \case
      FCMove x -> Just x
      _ -> Nothing
    copyEv = fforMaybe ev $ \case
      FCCopy x -> Just x
      _ -> Nothing
    pasteEv = fforMaybe ev $ \case
      FCPaste x -> Just x
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
        , _pfc_deleteElts  = deleteEv
        , _pfc_manipulate = manipEv
        , _pfc_resizeCanvas = resizeCanvasEv
        , _pfc_moveElt = moveEv
        , _pfc_copy = copyEv
        , _pfc_paste = pasteEv
        , _pfc_undo       = undoEv
        , _pfc_redo       = redoEv
        , _pfc_load = loadEv
        , _pfc_save = saveEv
      }
  pfo <- holdPF pfc
  let
    beh_pFState = _pfo_pFState pfo
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




-- | correct selection to include folder pairs
folderizeSelection :: SEltTree -> [LayerPos] -> [LayerPos]
folderizeSelection stree lps = scopeSelection scopeFn (Seq.fromList stree) lps where
  scopeFn :: SEltLabel -> Maybe Bool
  scopeFn (SEltLabel _ selt)= case selt of
    SEltFolderStart -> Just True
    SEltFolderEnd   -> Just False
    _               -> Nothing

-- don't need Has' constraint, but leaving here as reference because you do need it in some other circumstances...
randomActionFCmd ::
 (R.MonadRandom m, Has' Show CTag Identity)
 => Bool -> SEltTree -> m FCmd
randomActionFCmd doundo stree = do
  let
    nElts = length stree
    -- if we ever want to do this with elements only I guess, not sure why I commented this out
    --eltsOnly = filter (isElement . snd) $  L.indexed stree
    eltsOnly = L.indexed stree
    startCmd = if doundo then 0 else 2
    -- TODO we need to be able to test copy paste, but copy is not an undoable action..
    -- so really we need some cute trick to copy and paste in one command for the sake of testing...
    -- FCCopyPasta?
    endCmd = 6
  rcmd <- if null eltsOnly
    then return (2 :: Int)
    else R.getRandomR (startCmd, endCmd)
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
    -- resize the canvas
    3 -> do
      p1 <- randomXY
      p2 <- randomXY
      return $ FCResizeCanvas $ DeltaLBox p1 p2
    -- modify an existing element
    _ -> do

      -- choose just one random element for delete (so that we don't delete too many)
      rindex <- R.getRandomR (0, length eltsOnly - 1)
      let (deletePos, (SEltLabel _ _)) = eltsOnly L.!! rindex

      -- TODO this is sloppy, clean this up so selsection criterion for each type of command is more clear...
      -- many random elements
      shuffled <- shuffleM eltsOnly
      nTake <- R.getRandomR (1, length eltsOnly)
      nTakeFewer <- R.getRandomR (1, min (length eltsOnly) 10)
      let
        randomElts = L.take nTake shuffled
        randomEltsFewer = L.take nTakeFewer shuffled
        -- for moving elts around, we need to folderize selection to ensure scoping property after move
        -- TODO use fewer version for copy pasta
        --randomEltsScoped = folderizeSelection stree (map fst randomEltsFewer)
        randomEltsScoped = folderizeSelection stree (map fst randomElts)
      someTargetPos <- R.getRandomR (0, nElts-1)


      case rcmd of
        -- folderize delete to ensure scoping property
        4 -> return $ FCDeleteElts (folderizeSelection stree [deletePos])
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
        6 -> return $ FCMove (randomEltsScoped, someTargetPos)
        -- TODO these aren't currently being tested
        7 -> return $ FCCopy randomEltsScoped
        8 -> return $ FCPaste someTargetPos
        _ -> error "this should never happen"
